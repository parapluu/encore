#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <inttypes.h>
#include "pony_extensions.h"
#include "future.h"

extern scheduler_t* scheduler;
extern uint32_t scheduler_count;
extern volatile uint32_t quiet;
extern void* run_thread(void* arg); // in scheduler.h
extern void future_run_loop_start(); // in future.h
extern void lazy_t_init_current(); // in tit_lazy.h

struct scheduler_t
{
  pthread_t tid;
  uint32_t id;
  uint32_t cpu;
  uint32_t steal;
  uint64_t last;
  uint64_t app_msgs;
  uint64_t rc_msgs;
  bool quiet;
  bool finish;
  mpmcq_t q;
} __attribute__((aligned (64)));

// Tobias' additions -- to be refactored out
static __thread scheduler_t *scheduler_for_this_thread;
static __thread bool _just_suspended;

bool just_suspended() {
  return _just_suspended;
}

void set_just_suspended(bool value) {
  _just_suspended = value;
}

void ext_new_thread() {
  // XXX: For now, place this here to make sure that futures are always initialised
  init_futures(2, LAZY);
  lazy_t_init_current();
}

scheduler_t *ext_run_thread_start(scheduler_t *s) {
  if (s) {
    scheduler_for_this_thread = s;
  }
  return scheduler_for_this_thread;
}

void ext_run_loop_start() {
  future_run_loop_start();
}

bool in_main_thread() {
  return scheduler_for_this_thread == &scheduler[0];
}

void run_thread_restart() {
  set_just_suspended(true);
  run_thread(scheduler_for_this_thread);
  if (in_main_thread()) 
    future_run_loop_start();
}

static inline pony_actor_t* scheduler_worksteal(scheduler_t* s)
{
  scheduler_t* from;

  do
  {
    s->steal = rand_r(&s->steal);
    from = &scheduler[s->steal % scheduler_count];
  } while(from == s);

  return mpmcq_pop(&from->q);
}

pony_actor_t* ext_next_actor(scheduler_t* s, pony_actor_t* prev)
{
  pony_actor_t* next = mpmcq_pop(&s->q);

  if((next == NULL) && (scheduler_count > 1))
  {
    next = scheduler_worksteal(s);
  }

  if(next != NULL)
  {

    if(prev != NULL)
    {
      mpmcq_push(&s->q, prev);
    } else if(just_suspended()) {
      // prev is null because we just suspended, so we should not do anything
    } else if(s->quiet) {
      s->quiet = false;
      __sync_fetch_and_sub(&quiet, 1);
    }

    set_just_suspended(false);
    return next;
  }

  if(prev == NULL && !s->quiet) 
  {
    // If prev is null because we just suspended, then don't quiet this scheduler
    if(!just_suspended()) {
      s->quiet = true;
      __sync_fetch_and_add(&quiet, 1);
    }
  }

  set_just_suspended(false);
  return prev;
}
