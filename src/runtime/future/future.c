#define DEBUG_PRINT
#define _XOPEN_SOURCE

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <pthread.h>
#include "tit_lazy.h"
#include "tit_eager.h"
#include "future.h"
#include "future_actor.h"
#include "actor_def.h"
#include "pony_extensions.h"

struct future {
  pony_actor_t actor;
};

struct resumable {
  // strategy_t strategy; // Possible future work: support multiple strategies in parallel
  union {
    lazy_tit_t *lazy;
    eager_tit_t *eager;
  };
};

static strategy_t strategy;

static inline void suspend(resumable_t *r) {
  switch (strategy) {
  case LAZY:
    fork_lazy(run_thread_restart);
    break;
  case EAGER:
    suspend_eager(r->eager);
    break;
  default:
    assert(false);
  }
}

static resumable_t *mk_resumeable() {
  resumable_t *r = calloc(1, sizeof(resumable_t));
  switch (strategy) {
  case LAZY:
    r->lazy = lazy_t_get_current();
    break;
  case EAGER:
    r->eager = get_suspendable_tit();
    break;
  default:
    assert(false);
  }
  return r;
}

void future_resume(resumable_t *r) {
  switch (strategy) {
  case LAZY:
    resume_lazy(r->lazy);
    break;
  case EAGER:
    resume_eager(r->eager);
    break;
  default:
    assert(false);
  }
}

future_t *future_mk() {
  return (future_t*) future_create();
}

void future_chain(future_t *f, pony_actor_t* a, struct closure *c) {
  pony_arg_t argv[2];
  argv[0].p = a;
  argv[1].p = c;
#ifdef DEBUG_PRINT
  fprintf(stderr, "[%p]\t%p <--- chain (%p) from %p\n", pthread_self(), f, c, a);
#endif
  pony_sendv(&f->actor, FUT_MSG_CHAIN, 2, argv);
}

void future_block(future_t *f, pony_actor_t* a) {
  resumable_t *r = mk_resumeable();
  pony_arg_t argv[2];
  argv[0].p = a;
  argv[1].p = r;

#ifdef DEBUG_PRINT
  fprintf(stderr, "[%p]\t%p <--- block (%p) from %p \n", pthread_self(), f, argv[1].p, a);
#endif
  pony_sendv(&f->actor, FUT_MSG_BLOCK, 2, argv);

  suspend(r);
}

void future_await(future_t *f, pony_actor_t* a) {
  resumable_t *r = mk_resumeable();
  pony_arg_t argv[2];
  argv[0].p = a;
  argv[1].p = r;

#ifdef DEBUG_PRINT
  fprintf(stderr, "[%p]\t%p <--- await (%p) from %p \n", pthread_self(), f, argv[1].p, a);
#endif
  pony_sendv(&f->actor, FUT_MSG_AWAIT, 2, argv);

  suspend(r);
}

void yield(pony_actor_t* a) {
  resumable_t *r = mk_resumeable();
  pony_arg_t argv[1];
  argv[0].p = r;

#ifdef DEBUG_PRINT
  fprintf(stderr, "[%p]\t%p <--- yield (%p)\n", pthread_self(), a, argv[0].p);
#endif
  pony_sendv(a, FUT_MSG_RESUME, 1, argv);

  suspend(r);
}

inline volatile bool future_fulfilled(future_t *fut) {
  return future_actor_get_fulfilled(&fut->actor);
}

inline volatile void *future_read_value(future_t *fut) {
  return future_actor_get_value(&fut->actor);
}

volatile void *future_get(future_t *fut, pony_actor_t* actor) {
  volatile void *result;
  if (future_actor_get_value_and_fulfillment(&fut->actor, &result)) {
    return result;
  }
  future_block(fut, actor);
  return future_read_value(fut);
}

void future_fulfil(future_t *f, volatile void *value) {
  future_actor_set_value(&f->actor, value);
#ifdef DEBUG_PRINT
  fprintf(stderr, "[%p]\t%p <--- fulfil\n", pthread_self(), f);
#endif
  pony_send(&f->actor, FUT_MSG_FULFIL);
}

void init_futures(int cache_size, strategy_t s) {
  static bool init = false;
  if (init == false) {
    strategy = s;
    init_system(cache_size);
    init = true;
  }
}

void future_run_loop_start() {
  if (strategy == LAZY) {
    lazy_tit_t *current = lazy_t_get_current();
    if (current) {
      done_lazy(current);
    }
  }
}
