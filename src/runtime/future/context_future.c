#include "context.h"
#include "future.h"
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

#include "future_actor.h"

#define SET_BOOL(ptr, val) __sync_bool_compare_and_swap(ptr, !(val), val)
#define GET_BOOL(ptr) __sync_fetch_and_add(ptr, 0)

#define GET_PTR(ptr) __sync_fetch_and_add(ptr, 0)
#define SET_PTR(ptr,val) __sync_lock_test_and_set(ptr, val)

#define NOT_IMPL(str, explanation) { fprintf(stderr,"%s is not implemented: %s!\n", str, explanation); exit(1); }

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

bool fulfilled(future *fut)
{
  return GET_BOOL(&(fut->payload.fulfilled));
}

void *getValue(future *fut)
{
  return GET_PTR(&(fut->payload.value));
}

void chain(future *fut, pony_actor_t *actor, struct closure *closure)
{
  pony_arg_t argv[2] = {{.p = actor}, {.p = closure}};
  pony_sendv(&fut->actor, FUT_MSG_CHAIN, 2, argv);
}

void fulfil(future *fut, void *result)
{
  printf("Fulfil with %i\n", *((int*)result));
  SET_PTR(&(fut->payload.value), result);
  SET_BOOL(&(fut->payload.fulfilled),true);
  
  pony_send(&(fut->actor), FUT_MSG_FULFIL);
}

void block(future *fut, pony_actor_t* actor)
{
  NOT_IMPL("chain", "to await or to block is the client's business");
}

void await(future *fut, pony_actor_t* actor)
{
  NOT_IMPL("await", "to await or to block is the client's business");
}

void yield(pony_actor_t* actor)
{
  NOT_IMPL("yield", "");
}
