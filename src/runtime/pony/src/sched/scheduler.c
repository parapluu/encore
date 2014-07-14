#include "scheduler.h"
#include "cpu.h"
#include "mpmcq.h"
#include "../actor/actor.h"
#include "../gc/cycle.h"
#include <pthread.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

typedef struct scheduler_t
{
  pthread_t tid;
  uint32_t cpu;
  bool finish;
  bool forcecd;

  pony_actor_t* head;
  pony_actor_t* tail;

  struct scheduler_t* victim;

  // the following are accessed by other scheduler threads
  struct scheduler_t* thief __attribute__((aligned (64)));
  uint32_t waiting; //in the process of work stealing?
} scheduler_t __attribute__((aligned (64)));

// scheduler global data
static uint32_t scheduler_count;
static uint32_t scheduler_waiting; //schedulers waiting, globally
static scheduler_t* scheduler;
static bool finish;
static bool terminate;

static mpmcq_t inject;

static __thread scheduler_t* this_scheduler;
static __thread bool might_need_sync;

// forward declaration
static void push(scheduler_t* sched, pony_actor_t* actor);

/**
 * Takes all actors off the injection queue and puts them on the scheduler list
 */
static void handle_inject(scheduler_t* sched)
{
  pony_actor_t* actor;

  while((actor = mpmcq_pop(&inject)) != NULL)
    push(sched, actor);
}

/**
 * Gets the next actor from the scheduler queue.
 */
static pony_actor_t* pop(scheduler_t* sched)
{
  // clear the injection queue
  handle_inject(sched);

  pony_actor_t* actor = sched->tail;

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
 * If we can terminate, return true. If all schedulers are waiting, one of
 * them will tell the cycle detector to try to terminate.
 */
static bool quiescent(scheduler_t* sched)
{
  if(!__atomic_load_n(&finish, __ATOMIC_RELAXED))
    return false;

  if(__atomic_load_n(&terminate, __ATOMIC_RELAXED))
    return true;

  if(sched->finish)
  {
    uint32_t waiting = __atomic_load_n(&scheduler_waiting, __ATOMIC_RELAXED);

    // under these circumstances, the CD will always go on the current
    // scheduler.
    if(waiting == scheduler_count)
    {
      // it's safe to manipulate our victim, since we know it's paused as well
      if(sched->victim != NULL)
        __atomic_store_n(&sched->victim->thief, NULL, __ATOMIC_RELEASE);

      __atomic_store_n(&sched->waiting, 0, __ATOMIC_RELEASE);
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

    if(__atomic_compare_exchange_n(&victim->thief, &thief, sched, false,
      __ATOMIC_RELAXED, __ATOMIC_RELAXED))
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
  bool block = __atomic_compare_exchange_n(&sched->thief, &thief, (void*)1,
    false, __ATOMIC_RELAXED, __ATOMIC_RELAXED);

  __atomic_add_fetch(&scheduler_waiting, 1, __ATOMIC_RELAXED);

  uint64_t tsc = cpu_rdtsc();
  pony_actor_t* actor;

  while(true)
  {
    __atomic_store_n(&sched->waiting, 1, __ATOMIC_RELEASE);
    scheduler_t* victim = choose_victim(sched);

    if(victim != NULL)
    {
      while(__atomic_load_n(&sched->waiting, __ATOMIC_ACQUIRE) == 1)
      {
        if(cpu_core_pause(tsc) && quiescent(sched))
          return NULL;
      }

      sched->victim = NULL;
    } else {
      if(cpu_core_pause(tsc) && quiescent(sched))
        return NULL;
    }

    if((actor = pop(sched)) != NULL)
      break;
  }

  __atomic_sub_fetch(&scheduler_waiting, 1, __ATOMIC_RELAXED);

  if(block)
  {
    thief = (void*)1;

    __atomic_compare_exchange_n(&sched->thief, &thief, NULL, false,
      __ATOMIC_RELAXED, __ATOMIC_RELAXED);
  }

  return actor;
}

/**
 * Check if we have a thief. If we do, try to give it an actor. Signal the
 * thief to continue whether or not we gave it an actor.
 */
static void respond(scheduler_t* sched)
{
  scheduler_t* thief = __atomic_load_n(&sched->thief, __ATOMIC_RELAXED);

  if(thief == NULL)
    return;

  pony_actor_t* actor = pop(sched);

  if(actor != NULL)
  {
    assert(thief->waiting == 1);
    push(thief, actor);
  }

  __atomic_store_n(&thief->waiting, 0, __ATOMIC_RELEASE);
  assert(sched->thief == thief);
  __atomic_store_n(&sched->thief, NULL, __ATOMIC_RELEASE);
}

/**
 * Run a scheduler thread until termination.
 */
static void run(scheduler_t* sched)
{
  while(true)
  {
    //XXX: Temporary during upgrade to new ponyrt
    // If started in other thread, need to update sched
    if (might_need_sync) {
      sched = this_scheduler;
      might_need_sync = false;
    }

    // get an actor from our queue
    pony_actor_t* actor = pop(sched);

    if(actor == NULL)
    {
      // wait until we get an actor
      actor = request(sched);

      // termination
      if(actor == NULL)
        return;
    } else {
      // respond to our thief. we hold an actor for ourself, to make sure we
      // never give away our last actor.

      // TODO: if we're currently processing a system actor, we might give
      // away something we'd prefer to stay local.
      respond(sched);
    }

    // if this returns true, reschedule the actor on our queue
    might_need_sync = true;
    if(actor_run(actor))
      push(sched, actor);
  }
}

//XXX: Temporary during upgrade to new ponyrt
extern void future_run_loop_start(); // in future.c
void run_restart() {
  run(this_scheduler);
  if (this_scheduler == &scheduler[0]) 
    future_run_loop_start();
}

static void* run_thread(void* arg)
{
  scheduler_t* sched = arg;
  this_scheduler = sched;
  cpu_affinity(sched->cpu);
  run(sched);

  return NULL;
}

void scheduler_init(uint32_t threads, bool forcecd)
{
  uint32_t physical, logical;
  cpu_count(&physical, &logical);
  assert(physical <= logical);

  // if no thread count is specified, use the physical core count
  if(threads == 0)
    threads = physical;

  scheduler_count = threads;
  __atomic_store_n(&scheduler_waiting, 0, __ATOMIC_RELAXED);

  scheduler = calloc(scheduler_count, sizeof(scheduler_t));

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

bool scheduler_run(bool spinoff)
{
  __atomic_store_n(&finish, !spinoff, __ATOMIC_RELAXED);

  uint32_t start;

  if(spinoff)
  {
    start = 0;
  } else {
    start = 1;
    scheduler[0].tid = pthread_self();
  }

  for(uint32_t i = start; i < scheduler_count; i++)
  {
    if(pthread_create(&scheduler[i].tid, NULL, run_thread, &scheduler[i]) != 0)
      return false;
  }

  if(!spinoff)
    run_thread(&scheduler[0]);

  return true;
}

void scheduler_finish()
{
  uint32_t start;

  if(scheduler[0].tid == pthread_self())
  {
    start = 1;
  } else {
    start = 0;
    __atomic_store_n(&finish, true, __ATOMIC_RELAXED);
  }

  for(uint32_t i = start; i < scheduler_count; i++)
    pthread_join(scheduler[i].tid, NULL);

  __atomic_store_n(&finish, false, __ATOMIC_RELAXED);
  __atomic_store_n(&terminate, false, __ATOMIC_RELAXED);
  __atomic_store_n(&scheduler_waiting, 0, __ATOMIC_RELAXED);

  free(scheduler);
  scheduler = NULL;
  scheduler_count = 0;

  mpmcq_destroy(&inject);
}

pony_actor_t* scheduler_worksteal()
{
  // TODO: is this right?
  return pop(this_scheduler);
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
  __atomic_store_n(&terminate, true, __ATOMIC_RELAXED);
}

uint32_t scheduler_cores()
{
  return scheduler_count;
}
