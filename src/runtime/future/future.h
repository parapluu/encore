#ifndef __future_using_actors_h
#define __future_using_actors_h

#include <pony.h>
#include "closure.h"

typedef struct future future_t;

extern pony_type_t future_type;
void future_trace(pony_ctx_t *ctx, void* p);
pony_type_t *future_get_type(future_t *fut);
// ===============================================================
// Create, inspect and fulfil
// ===============================================================
future_t *future_mk(pony_ctx_t **ctx, pony_type_t *type);
bool         future_fulfilled  (future_t *fut);
void future_fulfil(pony_ctx_t **ctx, future_t *fut, encore_arg_t value);

// ===============================================================
// Means for actors to get, block and chain
// ===============================================================
encore_arg_t future_get_actor(pony_ctx_t **ctx, future_t *fut);
future_t *future_chain_actor(pony_ctx_t **ctx, future_t *fut, pony_type_t *type,
        closure_t *c);

// ===============================================================
// Possibly these functions do not belong in the future library
// ===============================================================
void future_await(pony_ctx_t **ctx, future_t *fut);
encore_arg_t run_closure(pony_ctx_t **ctx, closure_t *c, encore_arg_t value);
#endif
