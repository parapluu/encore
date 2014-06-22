#ifndef __future_using_actors_h
#define __future_using_actors_h

#include "pony/pony.h"
#include "tit_eager.h"
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

future_t *create_new_future();
volatile bool fulfilled(future_t *future);
volatile void *get_value(future_t *future);
void fulfil(future_t *future, volatile void *value);

// =============================================================================
// Actor-specific parts of the future library
// =============================================================================

void chain(future_t *future, pony_actor_t* actor_owning_closure, struct closure *closure);
void await(future_t *future, pony_actor_t* actor_awaiting_future);
void block(future_t *future, pony_actor_t* actor_blocking_on_future, void *stacklet);
volatile void *get_value_or_block(future_t *future, pony_actor_t* actor_blocking_on_future);

// =============================================================================
// Task-specific parts of the future library
// =============================================================================

/* TODO */

// =============================================================================
// Stuff that should be moved to some other library in the future
// =============================================================================

void yield(pony_actor_t *actor);
void init_futures(int cache_size, strategy_t strategy);
void future_run_loop_start();
void resume(resumable_t *r);

#endif
