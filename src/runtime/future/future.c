#define _XOPEN_SOURCE

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <pthread.h>
#include <ucontext.h>
#include "future.h"
#include "future_actor.h"
#include "actor_def.h"

struct future {
  pony_actor_t actor;
};

future_t *create_new_future() {
  future_t *fut = (future_t*) future_create();
  return fut;
}

void chain(future_t *f, pony_actor_t* a, struct closure *c) {
  pony_arg_t argv[2];
  argv[0].p = a;
  argv[1].p = c;
  fprintf(stderr, "[%p]\t%p <--- chain (%p) from %p\n", pthread_self(), f, c, a);
  pony_sendv(&f->actor, FUT_MSG_CHAIN, 2, argv);
}

void block(future_t *f, pony_actor_t* a, void *s) {
  if (!s) s = t_init();

  pony_arg_t argv[1];
  argv[0].p = a;
  argv[1].p = s;
  fprintf(stderr, "[%p]\t%p <--- block (%p) from %p \n", pthread_self(), f, s, a);

  // XXX: When self-send below is in place, no need to send the stacklet to the future actor
  pony_sendv(&f->actor, FUT_MSG_BLOCK, 2, argv);

  // XXX: Waiting on Sylvan's implementation of this
  // pony_block_actor(a);
  // XXX: Waiting on Sylvan's implementation of this
  // pony_sendp_FRONT_OF_QUE(a, FUT_MSG_RESUME, s);

  t_suspend((tit_t*) s);
}

void await(future_t *f, pony_actor_t* a) {
  tit_t *s = t_init();

  pony_arg_t argv[2];
  argv[0].p = a;
  argv[1].p = s;
  fprintf(stderr, "[%p]\t%p <--- block (%p) from %p \n", pthread_self(), f, s, a);
  pony_sendv(&f->actor, FUT_MSG_AWAIT, 2, argv);

  t_suspend(s);
}

void yield(pony_actor_t* a) {
  tit_t *s = t_init();

  pony_arg_t argv[1];
  argv[0].p = s;
  fprintf(stderr, "[%p]\t%p <--- yield (%p)\n", pthread_self(), a, s);
  pony_sendv(a, FUT_MSG_RESUME, 1, argv);

  t_suspend(s);
}

volatile bool fulfilled(future_t *fut) {
  return future_actor_get_fulfilled(&fut->actor);
}

volatile void *get_value(future_t *fut) {
  return future_actor_get_value(&fut->actor);
}

volatile void *get_value_or_block(future_t *fut, pony_actor_t* actor) {
  if (fulfilled(fut) == false) {
    block(fut, actor, NULL);
  }
  return get_value(fut);
}

void fulfil(future_t *f, volatile void *value) {
  future_actor_set_value(&f->actor, value);
  future_actor_set_fulfilled(&f->actor, true);
  fprintf(stderr, "[%p]\t%p <--- fulfil\n", pthread_self(), f);
  pony_send(&f->actor, FUT_MSG_FULFIL);
}
