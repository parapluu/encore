#include "task.h"
#include <future.h>

// relies on function translation from module Task.enc
extern future_t* _enc__global_fun_Taskspawn(pony_ctx_t** _ctx,
                                            pony_type_t** runtimeType,
                                            closure_t* _enc__arg_task);

promise_s* spawn_task(pony_ctx_t** ctx,
                      pony_type_t** runtimeType,
                      closure_t* task_closure)
{
  return (void*) _enc__global_fun_Taskspawn(ctx, runtimeType, task_closure);
}

encore_arg_t task_get_value(pony_ctx_t ** ctx, promise_s *promise)
{
  return future_get_actor(ctx, promise);
}

promise_s* task_chain_callback(pony_ctx_t **ctx,
                               promise_s *p,
                               pony_type_t *type,
                               closure_t *c)
{
  return future_chain_actor(ctx, p, type, c);
}

void promise_await(pony_ctx_t **ctx, promise_s *p)
{
  future_await(ctx, p);
}
