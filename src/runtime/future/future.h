#ifndef __future_using_actors_h
#define __future_using_actors_h

#include <pony.h>
#include <closure.h>

typedef enum future_class {
  FUTURE,
  VANILLA_FUTURE,
  POLY_VANILLA_FUTURE,
} future_class_t ;

typedef struct future_tnode {
  pony_actor_t * actor;
  ucontext_t *awaited_uctx;
  struct future_tnode * next;
  bool isget;
} future_tnode_t;

typedef struct future_tnode_info {
  void * fut;
  ucontext_t * awaited_uctx;
  future_class_t fclass;
  bool isget;
} future_tnode_info_t;

typedef struct closure_entry closure_entry_t;

/* TREIBER STACK API */

void treiber_stack_push(future_tnode_t **head, pony_actor_t* a, ucontext_t *awaited_uctx, int futop);
future_tnode_t * treiber_stack_pop(future_tnode_t **head);

/* STANDARD FUTURE API  */

typedef struct future {
  future_tnode_t *blocking_stack;
  closure_entry_t *chain_stack;
  const pony_type_t *type;
  encore_arg_t value;
  bool fulfilled;
} future_t;

void handle_future(encore_actor_t *actor, pony_ctx_t * futctx, void * pony_node);

void future_trace(pony_ctx_t *ctx, void* p);

typedef struct future future_t;

extern pony_type_t future_type;
void future_trace(pony_ctx_t *ctx, void* p);

/*
 * Get the value type of the future
 */
pony_type_t *future_get_type(future_t *fut);

// ===============================================================
// Create, inspect and fulfil
// ===============================================================

/*
 * Create a new future
 */
future_t *future_mk(pony_ctx_t **ctx, pony_type_t *type);

/** Check if the future is already fulfilled
 *
 * (this operation may be blocking until the future is fulfilled)
 */
bool future_fulfilled  (future_t *fut);

/*
 * Fulfil the future with the given value
 */
void future_fulfil(pony_ctx_t **ctx, future_t *fut, encore_arg_t value);

// ===============================================================
// Means for actors to get, block and chain
// ===============================================================

/** Gets the value from the future
 *
 * This functions extracts the value of the future. If the future is not
 * fulfilled by the time this function is called, the actor will be blocked
 * and unable to process messages until the future is fulfilled. When the future
 * is fulfilled, this operation just extracts the value contained in the future.
 */
encore_arg_t future_get_actor(pony_ctx_t **ctx, future_t *fut);

/*
 * Chains a callback to a future returning a new future (placeholder)
 * with the result of the callback function
 */
future_t *future_chain_actor(pony_ctx_t **ctx, future_t *fut, pony_type_t *type,
        closure_t *c);

void future_chain_forward(pony_ctx_t **ctx, future_t *fut, pony_type_t *type,
closure_t *c, future_t *r);


/** Registers a callback and returns void
 *
 *
 * This function attaches a callback on future `fut`. The void return value
 * makes this function useful for effectful computations with less overhead
 * than `future_chain_actor`.
 */
void future_register_callback(pony_ctx_t **ctx,
                              future_t *fut,
                              closure_t *c);

// ===============================================================
// Possibly these functions do not belong in the future library
// ===============================================================

/** Await on future until it is fulfilled
 *
 * This operation awaits on the future until it is fulfilled. In contrast to
 * `future_get_actor`, this operation does not block the current actor and just
 * puts on hold the processing of this message.
 */
void future_await(pony_ctx_t **ctx, future_t *fut);

extern pony_type_t future_type;

void future_discharge(pony_ctx_t **ctx, future_t *fut);

/* VANILLA FUTURE API */

typedef struct vanilla_future {
  const pony_type_t *type;
  pony_actor_t *actor;
  encore_arg_t value;
  bool fulfilled;
  bool blocking;
} vanilla_future_t;

encore_arg_t vanilla_future_get_actor(pony_ctx_t **ctx, vanilla_future_t *fut);
void vanilla_future_fulfil(pony_ctx_t **ctx, vanilla_future_t *fut, encore_arg_t value);
vanilla_future_t *vanilla_future_mk(pony_ctx_t **ctx, pony_type_t *type); 

/* POLY VANILLA FUTURE API */

typedef struct poly_vanilla_future {
  future_tnode_t *blocking_stack;
  const pony_type_t *type;
  encore_arg_t value;
  bool fulfilled;
} poly_vanilla_future_t;

void poly_vanilla_future_discharge(pony_ctx_t **ctx, poly_vanilla_future_t *fut);
encore_arg_t poly_vanilla_future_get_actor(pony_ctx_t **ctx, poly_vanilla_future_t *fut);
void poly_vanilla_future_fulfil(pony_ctx_t **ctx, poly_vanilla_future_t *fut, encore_arg_t value);
poly_vanilla_future_t *poly_vanilla_future_mk(pony_ctx_t **ctx, pony_type_t *type); 

#endif
