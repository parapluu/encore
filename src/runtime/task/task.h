#ifndef task_h
#define task_h

#include <pony.h>
#include <closure.h>

typedef void promise_s;

/** Spawns a task to eventually run the task_closure
 *
 * Spawns a task to run the closure. The return value is a promise
 * (similar to a future except task is not attached to an actor).
 */
promise_s* spawn_task(pony_ctx_t ** ctx,
                      pony_type_t ** runtimeType,
                      closure_t *task_closure);

/** Extracts the value from the task
 *
 * this is a blocking operation and the caller may be blocked if the task is
 * not yet fulfilled.
 */
encore_arg_t task_get_value(pony_ctx_t ** ctx, promise_s *promise);

/*
 * Chains a new callback to the promise
 */
promise_s *task_chain_callback(pony_ctx_t **ctx,
                               promise_s *p,
                               pony_type_t *type,
                               closure_t *c);

/*
 * Awaits until the promise has been fulfilled
 */
void promise_await(pony_ctx_t **ctx, promise_s *p);

#endif
