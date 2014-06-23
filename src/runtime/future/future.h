#ifndef __future_using_actors_h
#define __future_using_actors_h

#include "pony/pony.h"
#include "closure.h"

typedef struct future future_t;
typedef struct resumable resumable_t;

typedef enum {
  LAZY,
  EAGER
} strategy_t;


// =============================================================================
// Logic for creating, querying, and fulfilling futures
// =============================================================================

future_t *future_mk();
volatile bool future_fulfilled(future_t *future);
volatile void *future_read_value(future_t *future);
void future_fulfil(future_t *future, volatile void *value);

// =============================================================================
// Actor-specific parts of the future library
// =============================================================================

void future_chain(future_t *future, pony_actor_t* actor_owning_closure, struct closure *closure);
void future_await(future_t *future, pony_actor_t* actor_awaiting_future);
void future_block(future_t *future, pony_actor_t* actor_blocking_on_future);
volatile void *future_get(future_t *future, pony_actor_t* actor_blocking_on_future);

// =============================================================================
// Task-specific parts of the future library
// =============================================================================

/* TODO */

// =============================================================================
// Stuff that should be moved to some other library in the future
// =============================================================================

void yield(pony_actor_t *actor);

// =============================================================================
// This is low-level and should not be part of future public interface
// [ ] Refactor
// =============================================================================
void future_resume(resumable_t *r);
void future_run_loop_start();
void init_futures(int cache_size, strategy_t strategy);

#endif
