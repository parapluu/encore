#ifndef __future_using_actors_h
#define __future_using_actors_h

#include <pony/pony.h>
#include "closure.h"

typedef struct future future_t;

extern pony_type_t future_type;
void future_trace(void *);
pony_type_t *future_get_type(future_t *fut);
// ===============================================================
// Create, inspect and fulfil
// ===============================================================
future_t *future_mk(pony_type_t *type);
bool      future_fulfilled  (future_t *fut);
void     *future_read_value (future_t *fut);
void      future_fulfil     (future_t *fut, void *value);

// ===============================================================
// Means for actors to get, block and chain
// ===============================================================
void     *future_get_actor(future_t *fut);
future_t *future_chain_actor(future_t *fut, future_t* r, closure_t *c);
void      future_block_actor(future_t *fut); // TODO: does this belong in the public interface?
void      future_unblock_actor(future_t *fut);

// ===============================================================
// Possibly these functions do not belong in the future library
// ===============================================================
void future_suspend(void);
void future_suspend_resume(void *);
void future_await(future_t *);
void future_await_resume(void *);
void *run_closure(closure_t *c, void *value, future_t *fut);

#endif
