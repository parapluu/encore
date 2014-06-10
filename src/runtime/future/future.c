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

// This is a thread-local variable holding the context saved in the
// scheduler loop that we want to jump back into on a block
__thread volatile ucontext_t *scheduler_loop = NULL;
// FIXME: This is a hack which lets an actor test whether it is resuming or not
__thread volatile bool resuming = false;


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

void chain(future *fut, pony_actor_t* actor, void *closure) {
  pony_arg_t argv[2];
  argv[0].p = actor;
  argv[1].p = closure;
  fprintf(stderr, "[%p]\t%p <--- chain (%p) from %p\n", pthread_self(), fut, closure, actor);
  pony_sendv((pony_actor_t*) fut, FUT_MSG_CHAIN, 2, argv);
}

// XXX
extern void *get_q();

void block(future *fut, pony_actor_t* actor) {
  stacklet_t *context = t_init();

  pony_arg_t argv[2];
  argv[0].p = actor;
  argv[1].p = context;
  fprintf(stderr, "[%p]\t%p <--- block (%p) from %p \n", pthread_self(), fut, context, actor);
  pony_sendv((pony_actor_t*) fut, FUT_MSG_BLOCK, 2, argv);

  // If we are resumed here, we should simply return from here
  //CHECK_RESUME
  if (context->resumed) {
    fprintf(stderr, "[%p]\tResuming in blocking\n", pthread_self());
    return;
  } else {
    fprintf(stderr, "[%p]\tPassed the resume point in blocking without resuming\n", pthread_self());
  }

  t_restart(context, actor);
}

void await(future *fut, pony_actor_t* actor) {
  // TODO -- currently the same as block
}

void yield(pony_actor_t* self) {
  stacklet_t *context = t_init();

  pony_arg_t argv[1];
  argv[0].p = context;
  fprintf(stderr, "[%p]\t%p <--- yield (%p)\n", pthread_self(), self, context);
  pony_sendv(self, FUT_MSG_RESUME, 1, argv);

  t_restart(context, self);
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

void fulfil(future *fut, void *value) {
    // XXX: Need to make sure that the entire future payload is written atomically to memory, 
    // or at least that value is written *before* fulfilled is
    future_payload temp = { true, value }; 
    fut->payload = temp;
    pony_arg_t argv[1];
    argv[0].p = NULL;
    fprintf(stderr, "[%p]\t%p <--- fulfil\n", pthread_self(), fut);
    pony_sendv((pony_actor_t*) fut, FUT_MSG_FULFIL, 1, argv);
}
