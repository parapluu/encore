//#define DEBUG_PRINT 1

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <pthread.h>
#include "future.h"
#include "future_actor.h"
#include "pony/pony.h"
#include "tit_lazy.h"
#include "tit_eager.h"

// #include "ext/pony_extensions.h"

struct resumable {
  // strategy_t strategy; // Possible future work: support multiple strategies in parallel
  union {
    lazy_tit_t *lazy;
    eager_tit_t *eager;
  };
};

extern void run_restart();

static strategy_t strategy;

static inline void suspend(resumable_t *r) {
  switch (strategy) {
  case LAZY:
    fork_lazy(run_restart);
    break;
  case EAGER:
    suspend_eager(r->eager);
    break;
  default:
    assert(false);
  }
}

static resumable_t *mk_resumeable() {
  resumable_t *r = pony_alloc(sizeof(resumable_t));
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
  fprintf(stderr, "[%p]\t%p <--- chain (%p) from %p\n", (void *)pthread_self(), f, c, a);
#endif
  pony_sendv(f, FUT_MSG_CHAIN, 2, argv);
}

void future_block(future_t *f, pony_actor_t* a) {
  future_set_blocking(f);
  resumable_t *r = mk_resumeable();
  pony_arg_t argv[2];
  argv[0].p = a;
  argv[1].p = r;

#ifdef DEBUG_PRINT
  fprintf(stderr, "[%p]\t%p <--- block (%p) from %p \n", (void *)pthread_self(), f, argv[1].p, a);
#endif
  pony_sendv(f, FUT_MSG_BLOCK, 2, argv);

  pony_unschedule(a);
  suspend(r);
}

void future_await(future_t *f, pony_actor_t* a) {
  resumable_t *r = mk_resumeable();
  pony_arg_t argv[2];
  argv[0].p = a;
  argv[1].p = r;

#ifdef DEBUG_PRINT
  fprintf(stderr, "[%p]\t%p <--- await (%p) from %p \n", (void *)pthread_self(), f, argv[1].p, a);
#endif
  pony_sendv(f, FUT_MSG_AWAIT, 2, argv);

  suspend(r);
}

void yield(pony_actor_t* a) {
  resumable_t *r = mk_resumeable();
  pony_arg_t argv[1];
  argv[0].p = r;

#ifdef DEBUG_PRINT
  fprintf(stderr, "[%p]\t%p <--- yield (%p)\n", (void *)(void *)pthread_self(), a, argv[0].p);
#endif
  pony_sendv(a, FUT_MSG_RESUME, 1, argv);

  suspend(r);
}

inline bool future_fulfilled(future_t *fut) {
  return future_actor_get_fulfilled(fut);
}

inline void *future_read_value(future_t *fut) {
  return future_actor_get_value(fut);
}

void *future_get(future_t *fut, pony_actor_t* actor) {
  void *result;
  if (future_actor_get_value_and_fulfillment(fut, &result)) {
    return result;
  }
  future_block(fut, actor);
  return future_read_value(fut);
}

void future_fulfil(future_t *f, void *value) {
  future_actor_set_value(f, value);

#ifdef DEBUG_PRINT
  fprintf(stderr, "[%p]\t%p <--- fulfil\n", (void *)pthread_self(), f);
#endif

  if (future_has_blocking(f))
    pony_send(f, FUT_MSG_FULFIL);
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
