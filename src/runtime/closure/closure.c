#include "closure.h"
#include <assert.h>

pony_type_t closure_type = {
  .id = ID_CLOSURE,
  .size = sizeof(struct closure),
  .trace = closure_trace,
};

void closure_trace(pony_ctx_t *ctx, void *p)
{
  assert(p);
  closure_t *c = (closure_t *) p;
  pony_traceobject(ctx, c->env, c->trace);
}

closure_t *closure_mk(pony_ctx_t **ctx, closure_fun fn, void *env,
    pony_trace_fn trace)
{
  closure_t *c = encore_alloc(*ctx, sizeof(closure_t));
  c->call = fn;
  c->env = env;
  c->trace = trace;
  return c;
}

value_t closure_call(pony_ctx_t **ctx, closure_t *closure, value_t args[]){
  return closure->call(ctx, closure->runtimeTypes, args, closure->env);
}
