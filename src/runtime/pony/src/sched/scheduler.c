#define _XOPEN_SOURCE 800

#include "scheduler.h"
#include "cpu.h"
#include "mpmcq.h"
#include "../actor/actor.h"
#include "../gc/cycle.h"
#include "encore.h"
#include <string.h>
#include <stdio.h>
#include <signal.h>
#include <assert.h>

typedef struct scheduler_t scheduler_t;

__pony_spec_align__(
  struct scheduler_t
  {
    pony_thread_id_t tid;
    uint32_t cpu;
    bool finish;
    bool forcecd;

    pony_actor_t* head;
    pony_actor_t* tail;

    struct scheduler_t* victim;

    // the following are accessed by other scheduler threads
    __pony_spec_align__(scheduler_t* thief, 64);
    uint32_t waiting; //in the process of work stealing?
  }, 64
);

static DECLARE_THREAD_FN(run_thread);

// scheduler global data
static uint32_t scheduler_count;
static uint32_t scheduler_waiting; //schedulers waiting, global
static uint32_t context_waiting = 0;
static uint32_t thread_exit = 0;
static scheduler_t* scheduler;
static bool detect_quiescence;
static bool shutdown_on_stop;
static bool terminate;

static mpmcq_t inject;

static __pony_thread_local scheduler_t* this_scheduler;

// forward declaration
static void push(scheduler_t* sched, pony_actor_t* actor);

/**
 * Takes all actors off the injection queue and puts them on the scheduler list
 */
static void handle_inject(scheduler_t* sched)
{
  pony_actor_t* actor;

  while((actor = (pony_actor_t*) mpmcq_pop(&inject)) != NULL)
    push(sched, actor);
}

static __pony_thread_local int counter = 0;
/**
 * Gets the next actor from the scheduler queue.
 */
static pony_actor_t* pop(scheduler_t* sched)
{
  // clear the injection queue
  handle_inject(sched);

  pony_actor_t* actor;

  actor = sched->tail;

  if(actor != NULL)
  {
    if(actor != sched->head)
    {
      sched->tail = actor_next(actor);
    } else {
      sched->head = NULL;
      sched->tail = NULL;
    }

    actor_setnext(actor, NULL);
  }

  return actor;
}

/**
 * Puts an actor on the scheduler queue.
 */
static void push(scheduler_t* sched, pony_actor_t* actor)
{
  pony_actor_t* head = sched->head;

  if(head != NULL)
  {
    actor_setnext(head, actor);
    sched->head = actor;
  } else {
    sched->head = actor;
    sched->tail = actor;
  }
}

/**
 * Puts an actor on the scheduler queue in the beginning.
 */
static void push_first(scheduler_t* sched, pony_actor_t* actor)
{
  pony_actor_t* tail = sched->tail;

  if(tail != NULL)
  {
    actor_setnext(actor, tail);
    sched->tail = actor;
  } else {
    sched->head = actor;
    sched->tail = actor;
  }
}

/**
 * If we can terminate, return true. If all schedulers are waiting, one of
 * them will tell the cycle detector to try to terminate.
 */
static bool quiescent(scheduler_t* sched)
{
  if(!__pony_atomic_load_n(&detect_quiescence, PONY_ATOMIC_RELAXED,
    PONY_ATOMIC_NO_TYPE))
    return false;

  if(__pony_atomic_load_n(&terminate, PONY_ATOMIC_RELAXED,
    PONY_ATOMIC_NO_TYPE))
    return true;

  if(sched->finish)
  {
    uint32_t waiting = __pony_atomic_load_n(&scheduler_waiting,
      PONY_ATOMIC_RELAXED, PONY_ATOMIC_NO_TYPE);

    // under these circumstances, the CD will always go on the current
    // scheduler.
    if(waiting == scheduler_count)
    {
      // it's safe to manipulate our victim, since we know it's paused as well
      if(sched->victim != NULL)
      {
        __pony_atomic_store_n(&sched->victim->thief, NULL, PONY_ATOMIC_RELEASE,
          PONY_ATOMIC_NO_TYPE);
      }

      __pony_atomic_store_n(&sched->waiting, 0, PONY_ATOMIC_RELEASE,
        PONY_ATOMIC_NO_TYPE);

      cycle_terminate(sched->forcecd);
    }
  }

  return false;
}

static scheduler_t* choose_victim(scheduler_t* sched)
{
  if(scheduler_count == 1)
    return NULL;

  assert(sched->victim == NULL);

  scheduler_t* victim = sched;
  scheduler_t* first = victim;

  do
  {
    victim--;

    if(victim < scheduler)
      victim = &scheduler[scheduler_count - 1];

    scheduler_t* thief = NULL;

    if(__pony_atomic_compare_exchange_n(&victim->thief, &thief,
        sched, false, PONY_ATOMIC_RELAXED, PONY_ATOMIC_RELAXED, intptr_t))
    {
      sched->victim = victim;
      return victim;
    }
  } while(victim != first);

  return NULL;
}

/**
 * Wait until we receive a stolen actor. Tight spin at first, falling back to
 * nanosleep. Once we have fallen back, check for quiescence.
 */
static pony_actor_t* request(scheduler_t* sched)
{
  scheduler_t* thief = NULL;

  bool block = __pony_atomic_compare_exchange_n(&sched->thief, &thief,
    (void*)1, false, PONY_ATOMIC_RELAXED, PONY_ATOMIC_RELAXED, intptr_t);

  __pony_atomic_fetch_add(&scheduler_waiting, 1, PONY_ATOMIC_RELAXED, uint32_t);

  uint64_t tsc = cpu_rdtsc();
  pony_actor_t* actor;

  while(true)
  {
    __pony_atomic_store_n(&sched->waiting, 1, PONY_ATOMIC_RELEASE,
      PONY_ATOMIC_NO_TYPE);

    scheduler_t* victim = choose_victim(sched);

    if(victim != NULL)
    {
      while(__pony_atomic_load_n(&sched->waiting, PONY_ATOMIC_ACQUIRE,
        PONY_ATOMIC_NO_TYPE) == 1)
      {
        if(cpu_core_pause(tsc) && quiescent(sched)) {
          sched->victim = NULL;
          __pony_atomic_store_n(&sched->thief, NULL, PONY_ATOMIC_RELAXED,
              PONY_ATOMIC_NO_TYPE);
          return NULL;
        }
      }

      sched->victim = NULL;
    } else {
      if(cpu_core_pause(tsc) && quiescent(sched)) {
        __pony_atomic_store_n(&sched->thief, NULL, PONY_ATOMIC_RELAXED,
            PONY_ATOMIC_NO_TYPE);
        return NULL;
      }
    }

    if((actor = pop(sched)) != NULL)
      break;
  }

  if(block)
  {
    thief = (scheduler_t*)1;

    /** MSVC++ throws warning here - non-void expression with no effect.
     *  GCC/clang would throw a warning for unused result, hence disable the
     *  MSVC++ warning.
     *
     *  http://msdn.microsoft.com/en-us/library/577cze47.aspx
     */
#if defined(PLATFORM_IS_WINDOWS)
#  pragma warning(disable:4552)
#endif
    __pony_atomic_compare_exchange_n(&sched->thief, &thief, NULL, false,
      PONY_ATOMIC_RELAXED, PONY_ATOMIC_RELAXED, intptr_t);
  }

  return actor;
}

/**
 * Check if we have a thief. If we do, try to give it an actor. Signal the
 * thief to continue whether or not we gave it an actor.
 */
static void respond(scheduler_t* sched)
{
  scheduler_t* thief = (scheduler_t*)__pony_atomic_load_n(&sched->thief,
    PONY_ATOMIC_RELAXED, PONY_ATOMIC_NO_TYPE);

  assert(thief != (void*)1);

  if(thief == NULL)
    return;

  pony_actor_t* actor = pop(sched);

  if(actor != NULL)
  {
    assert(thief->waiting == 1);
    push(thief, actor);
    __pony_atomic_fetch_sub(&scheduler_waiting, 1, PONY_ATOMIC_RELAXED, uint32_t);
  }

  __pony_atomic_store_n(&thief->waiting, 0, PONY_ATOMIC_RELEASE,
    PONY_ATOMIC_NO_TYPE);

  assert(sched->thief == thief);

  __pony_atomic_store_n(&sched->thief, NULL, PONY_ATOMIC_RELEASE,
    PONY_ATOMIC_NO_TYPE);
}

/**
 * Run a scheduler thread until termination.
 */
static void run(scheduler_t* sched)
{
  while(true)
  {
    assert(sched == this_scheduler);
    // get an actor from our queue
    pony_actor_t* actor = pop(sched);

    if(actor == NULL)
    {
      // wait until we get an actor
      actor = request(sched);

      // termination
      if(actor == NULL) {
        assert(pop(sched) == NULL);
        return;
      }
    } else {
      // respond to our thief. we hold an actor for ourself, to make sure we
      // never give away our last actor.

      // TODO: if we're currently processing a system actor, we might give
      // away something we'd prefer to stay local.
      respond(sched);
    }

    // if this returns true, reschedule the actor on our queue
    if(actor_run(actor)) {
#ifdef LAZY_IMPL
      sched = this_scheduler;
#endif
      push(sched, actor);
    } else {
#ifndef LAZY_IMPL
      actor_unlock((encore_actor_t*)actor);
#endif
    }
  }
}

static void jump_buffer()
{
  __pony_atomic_fetch_add(&thread_exit, 1, PONY_ATOMIC_RELAXED, uint32_t);
  while(__pony_atomic_load_n(&thread_exit,
        PONY_ATOMIC_RELAXED, PONY_ATOMIC_NO_TYPE) < scheduler_count) ;
}

static void __attribute__ ((noreturn)) jump_origin()
{
  static __pony_thread_local char stack[MINSIGSTKSZ];
  ucontext_t ctx;
  getcontext(&ctx);
  ctx.uc_stack.ss_sp = stack;
  ctx.uc_stack.ss_size = MINSIGSTKSZ;
  ctx.uc_link = origin;
  makecontext(&ctx, (void(*)(void))&jump_buffer, 0);
  setcontext(&ctx);
  assert(0);
}

void __attribute__ ((noreturn)) public_run(pthread_mutex_t *lock)
{
  assert(lock);
  pthread_mutex_unlock(lock);
  assert(this_scheduler);
  run(this_scheduler);

  __pony_atomic_fetch_add(&context_waiting, 1, PONY_ATOMIC_RELAXED, uint32_t);
  while(__pony_atomic_load_n(&context_waiting,
        PONY_ATOMIC_RELAXED, PONY_ATOMIC_NO_TYPE) < scheduler_count) ;

  jump_origin();
}

static void *run_thread(void *arg)
{
  scheduler_t* sched = (scheduler_t*) arg;
  this_scheduler = sched;
  cpu_affinity(sched->cpu);

#ifdef LAZY_IMPL
  context ctx;
  this_context = &ctx;
  root = &ctx.ctx;
  ucontext_t ucontext;
  origin = &ucontext;

  getcontext(origin);
  if (__pony_atomic_load_n(&context_waiting,
        PONY_ATOMIC_RELAXED, PONY_ATOMIC_NO_TYPE) == scheduler_count) {
    return NULL;
  }
#endif

  run(sched);

#ifdef LAZY_IMPL
  __pony_atomic_fetch_add(&context_waiting, 1, PONY_ATOMIC_RELAXED, uint32_t);
  while(__pony_atomic_load_n(&context_waiting,
        PONY_ATOMIC_RELAXED, PONY_ATOMIC_NO_TYPE) < scheduler_count) ;

  jump_origin();
#endif

  return NULL;
}

static void scheduler_shutdown()
{
  uint32_t start;

  if(scheduler[0].tid == pony_thread_self())
    start = 1;
  else
    start = 0;

  for(uint32_t i = start; i < scheduler_count; i++) {
    pony_thread_join(scheduler[i].tid);
    assert(pop(&scheduler[i]) == NULL);
  }
  assert(pop(&scheduler[0]) == NULL);

  __pony_atomic_store_n(&detect_quiescence, false, PONY_ATOMIC_RELAXED,
    PONY_ATOMIC_NO_TYPE);

  __pony_atomic_store_n(&terminate, false, PONY_ATOMIC_RELAXED,
    PONY_ATOMIC_NO_TYPE);

  __pony_atomic_store_n(&scheduler_waiting, 0, PONY_ATOMIC_RELAXED,
    PONY_ATOMIC_NO_TYPE);

  free(scheduler);
  scheduler = NULL;
  scheduler_count = 0;

  mpmcq_destroy(&inject);
}

void scheduler_init(uint32_t threads, bool forcecd)
{
  uint32_t physical = 0;
  uint32_t logical = 0;

  cpu_count(&physical, &logical);

  assert(physical <= logical);

  // if no thread count is specified, use the physical core count
  if(threads == 0)
    threads = physical;

  scheduler_count = threads;
#if defined(SINGLE_THREAD_ON_MACOSX) && defined(PLATFORM_IS_MACOSX)
  scheduler_count = 1;
#endif
  scheduler_waiting = 0;
  scheduler = (scheduler_t*)calloc(scheduler_count, sizeof(scheduler_t));

  if(scheduler_count <= physical)
  {
    // assign threads to physical processors
    uint32_t index = 0;

    for(uint32_t i = 0; i < scheduler_count; i++)
    {
      if(cpu_physical(i))
      {
        scheduler[index].cpu = i;
        index++;
      }
    }
  } else {
    // assign threads to logical processors
    for(uint32_t i = 0; i < scheduler_count; i++)
      scheduler[i].cpu = i % logical;
  }

  scheduler[0].finish = true;
  scheduler[0].forcecd = forcecd;

  mpmcq_init(&inject);
}

bool scheduler_start(pony_termination_t termination)
{
  detect_quiescence = termination == PONY_DONT_WAIT;
  shutdown_on_stop = termination == PONY_ASYNC_WAIT;

  uint32_t start;

  if(termination == PONY_ASYNC_WAIT)
  {
    start = 0;
  } else {
    start = 1;
    scheduler[0].tid = pony_thread_self();
  }

  for(uint32_t i = start; i < scheduler_count; i++)
  {
    if(!pony_thread_create(&scheduler[i].tid, run_thread, &scheduler[i]))
      return false;
  }

  if(termination != PONY_ASYNC_WAIT)
  {
    run_thread(&scheduler[0]);
    scheduler_shutdown();
  }

  return true;
}

void scheduler_stop()
{
  __pony_atomic_store_n(&detect_quiescence, true, PONY_ATOMIC_RELAXED,
    PONY_ATOMIC_NO_TYPE);

  if(shutdown_on_stop)
    scheduler_shutdown();
}

pony_actor_t* scheduler_worksteal()
{
  // TODO: is this right?
  return pop(this_scheduler);
}

void scheduler_add_first(pony_actor_t* actor)
{
  assert(this_scheduler);
  push_first(this_scheduler, actor);
}

void scheduler_add(pony_actor_t* actor)
{
  if(this_scheduler != NULL)
  {
    // add to the current scheduler thread
    push(this_scheduler, actor);
  } else {
    // put on the shared mpmcq
    mpmcq_push(&inject, actor);
  }
}

void scheduler_terminate()
{
  __pony_atomic_store_n(&terminate, true, PONY_ATOMIC_RELAXED,
    PONY_ATOMIC_NO_TYPE);
}

uint32_t scheduler_cores()
{
  return scheduler_count;
}
