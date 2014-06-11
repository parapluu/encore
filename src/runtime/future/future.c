#define _XOPEN_SOURCE

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <pthread.h>
#include <ucontext.h>
#include "future.h"
#include "ccontext.h"
#include "future_actor.h"

extern pony_actor_type_t future_actor_type;

// This function is almost verbatim actor_create from actor.c, but
// with the important difference of reserving space for two fields
// at the end holding the future state. 
pony_actor_t* future_create() {
  int INDEX = sizeof(future_payload) + sizeof(future_actor_type);
  future *fut = (future*) pool_alloc(&pool_thread[INDEX], &pool_global[INDEX]);
  return actor_create_stage_two(&future_actor_type, &(fut->actor));
}
  
future *createNewFuture() {
  future *fut = (future*) future_create();
  fut->payload.fulfilled = false;
  fut->payload.value = NULL;
  return fut; 
}

void chain(future *f, pony_actor_t* a, void *c) {
  pony_arg_t argv[2];
  argv[0].p = a;
  argv[1].p = c;
  fprintf(stderr, "[%p]\t%p <--- chain (%p) from %p\n", pthread_self(), f, c, a);
  pony_sendv(&f->actor, FUT_MSG_CHAIN, 2, argv);
}

void block(future *f, pony_actor_t* a) {
  stacklet_t *s = t_init();

  pony_arg_t argv[2];
  argv[0].p = a;
  argv[1].p = s;
  fprintf(stderr, "[%p]\t%p <--- block (%p) from %p \n", pthread_self(), f, s, a);
  pony_sendv(&f->actor, FUT_MSG_BLOCK, 2, argv);

  t_restart(s, a);
}

extern void pony_actor_unblock(pony_actor_t *a);

void await(future *f, pony_actor_t* a) {
  stacklet_t *s = t_init();

  pony_arg_t argv[2];
  argv[0].p = a;
  argv[1].p = s;
  fprintf(stderr, "[%p]\t%p <--- block (%p) from %p \n", pthread_self(), f, s, a);
  pony_sendv(&f->actor, FUT_MSG_BLOCK, 2, argv);

  pony_actor_unblock(a);
  t_restart(s, a);
}

void yield(pony_actor_t* a) {
  stacklet_t *s = t_init();

  pony_arg_t argv[1];
  argv[0].p = s;
  fprintf(stderr, "[%p]\t%p <--- yield (%p)\n", pthread_self(), a, s);
  pony_sendv(a, FUT_MSG_RESUME, 1, argv);

  t_restart(s, a);
}

bool fulfilled(future *fut) {
  return fut->payload.fulfilled;
}

void *getValue(future *fut) {
  return fut->payload.value;
}

void *getValueOrBlock(future *fut, pony_actor_t* actor) {
  if (fulfilled(fut) == false) {
    block(fut, actor);
  }
  return fut->payload.value;
}

void fulfil(future *f, void *value) {
    // XXX: Need to make sure that the entire future payload is written atomically to memory, 
    // or at least that value is written *before* fulfilled is
    future_payload atom = { true, value }; 
    f->payload = atom;
    fprintf(stderr, "[%p]\t%p <--- fulfil\n", pthread_self(), f);
    pony_send((pony_actor_t*) f, FUT_MSG_FULFIL);
}
