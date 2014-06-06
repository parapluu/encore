/**
 *  @file future.h
 */

// ================================================================
// TODO
// ================================================================
// [x] Who garbage collects the future struct? 
// [x] Can the actor object be extended with an arbitrary payload?
// [ ] Refactor solution to creating a future actor
// ================================================================

#ifndef __future_using_actors_h
#define __future_using_actors_h

#include "pony/pony.h"
#include "actor.h"
#include "heap.h"
#include "pool.h"
#include "actorq.h"
#include "actor_def.h"

// The payload is a boolean flag that controls whether the future value has been set
// The value is the future's value
typedef struct future_payload {
  bool populated;
  void *value;
} future_payload;

// A future type is a subtype of a pony_actor_t which has a couple of extra bytes at the end
typedef struct future {
  pony_actor_t actor;
  volatile future_payload payload;
} future;

/**
 * Creates a new future object, including an actor to drive its logic
 */
future *createNewFuture(pony_actor_t* actor, heap_t* heap);
bool populated(future *fut);
void *getValue(future *fut);
void chain(future *fut, pony_actor_t* actor, void *closure);
void block(future *fut, pony_actor_t* actor);
void yield(future *fut, pony_actor_t* actor);
void fulfil(future *fut, void *value);

#endif
