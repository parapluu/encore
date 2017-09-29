#ifndef __future_using_actors_h
#define __future_using_actors_h

#include <pony.h>
#include "closure.h"

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

void future_chain_with_fut(pony_ctx_t **ctx, future_t *fut, pony_type_t *type,
        closure_t *c, future_t *r, bool keepFwd);

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
#endif
