#define _XOPEN_SOURCE

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <ucontext.h>
#include "future.h"
#include "context.h"
#include "future_actor.h"

extern pony_actor_type_t future_actor_type;
extern volatile bool hacky_global_blocked_flag;

#define CHECK_RESUME if (resuming) { resuming = false; return; }

// This is a thread-local variable holding the context saved in the
// scheduler loop that we want to jump back into on a block
__thread ucontext_t *scheduler_loop = NULL;
// FIXME: This is a hack which lets an actor test whether it is resuming or not
__thread volatile bool resuming = false;

void setResuming(volatile bool r) {
  resuming = r;
}

volatile bool getResuming() {
  return resuming;
}

void setSchedulerLoop(ucontext_t *c) {
  scheduler_loop = c;
}

bool getSchedulerLoop() {
  return scheduler_loop;
}

void saveSchedulerLoopContext() {
  ucontext_t *ctx = malloc(sizeof(ucontext_t));
  getcontext(ctx);
  setSchedulerLoop(ctx);
}


// This function is almost verbatim actor_create from actor.c, but
// with the important difference of reserving space for two fields
// at the end holding the future state. 
pony_actor_t* future_create() {
  int INDEX = sizeof(future_payload) + sizeof(future_actor_type);
  future *fut = (future*) pool_alloc(&pool_thread[INDEX], &pool_global[INDEX]);
  return actor_create_stage_two(&future_actor_type, &(fut->actor));
}
  
future *createNewFuture(pony_actor_t* actor) {
  future *fut = (future*) future_create();
  fut->payload.populated = false;
  fut->payload.value = NULL;
  return fut; 
}

void chain(future *fut, pony_actor_t* actor, void *closure) {
  pony_arg_t argv[2];
  argv[0].p = actor;
  argv[1].p = closure;
  pony_sendv(fut, FUT_MSG_CHAIN, 2, argv);
}

void block(future *fut, pony_actor_t* actor) {
  ucontext_t *ctx = malloc(sizeof(ucontext_t));
  getcontext(ctx);
  
  // If we are resumed here, we should simply return from here
  //CHECK_RESUME
  if (resuming) {
    resuming = false;
    fprintf(stderr, "Resuming in blocking\n");
    return;
  } else {
    fprintf(stderr, "Passed the resume point in blocking without resuming\n");
  }
  
  pony_arg_t argv[2];
  argv[0].p = actor;
  argv[1].p = ctx;
  pony_sendv(fut, FUT_MSG_BLOCK, 2, argv);

  hacky_global_blocked_flag = true;
  setResuming(true);
  setcontext(scheduler_loop);

  fprintf(stderr, "IF THIS TEXT IS EVER PRINTED, THEN THE YIELD BACK TO THE SCHEDULER FAILED\n");
  assert(false);

}

void yield(future *fut, pony_actor_t* actor) {
  ucontext_t *ctx = malloc(sizeof(ucontext_t));
  getcontext(ctx);

  // If we are resumed here, we should simply return from here
  CHECK_RESUME
 
  pony_arg_t argv[2];
  argv[0].p = actor;
  argv[1].p = ctx;
  pony_sendv(fut, FUT_MSG_YIELD, 2, argv);
  setResuming(true);
  setcontext(scheduler_loop);
  // XXX: call to suspend and put the actor back on the scheduler queue
}

bool populated(future *fut) {
  return fut->payload.populated;
}

void *getValue(future *fut) {
  return fut->payload.value;
}

void *getValueOrBlock(future *fut, pony_actor_t* actor) {
  if (populated(fut) == false) {
    block(fut, actor);
  }
  return fut->payload.value;
}

void fulfil(future *fut, void *value) {
    // XXX: Need to make sure that the entire future payload is written atomically to memory, 
    // or at least that value is written *before* populated is
    future_payload temp = { true, value }; 
    fut->payload = temp;
    pony_arg_t argv[1];
    argv[0].p = NULL;
    pony_sendv(fut, FUT_MSG_FULFIL, 1, argv);
}
