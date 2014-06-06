#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include "future.h"
#include "future_actor.h"

// This function is almost verbatim actor_create from actor.c, but
// with the important difference of reserving space for two fields
// at the end holding the future state. 
pony_actor_t* future_create() {
  int INDEX = sizeof(future_payload) + sizeof(future_actor_type);
  future *fut = (future*) pool_alloc(&pool_thread[INDEX], &pool_global[INDEX]);
  return actor_create_stage_two(&future_actor_type, &(fut->actor));
}
  
future *createNewFuture(pony_actor_t* actor, heap_t* heap) {
  future *fut = (future*) future_create();
  fut->payload.populated = false;
  fut->payload.value = NULL;
  return fut; 
}

void chain(future *fut, pony_actor_t* actor, void *closure) {
  pony_arg_t argv[2];
  argv[0].p = actor;
  argv[1].p = closure;
  pony_sendv(actor, MSG_CHAIN, 2, argv);
}

void block(future *fut, pony_actor_t* actor) {
  pony_arg_t argv[1];
  argv[0].p = actor;
  pony_sendv(actor, MSG_BLOCK, 1, argv);
  // XXX: call to suspend which does not put the actor back on the scheduler queue
}

void yield(future *fut, pony_actor_t* actor, void *closure) {
  pony_arg_t argv[2];
  argv[0].p = actor;
  argv[1].p = closure;
  pony_sendv(actor, MSG_YIELD, 2, argv);
  // XXX: call to suspend and put the actor back on the scheduler queue
}

bool populated(future *fut) {
  return fut->payload.populated;
}

void *getValue(future *fut) {
  return fut->payload.value;
}

void fulfil(future *fut, void *value) {
    // XXX: Need to make sure that the entire future payload is written atomically to memory, 
    // or at least that value is written *before* populated is
    future_payload temp = { true, value };
    fut->payload = temp;
}
