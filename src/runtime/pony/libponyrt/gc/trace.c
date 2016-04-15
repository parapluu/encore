#include "trace.h"
#include "gc.h"
#include "../sched/scheduler.h"
#include "../sched/cpu.h"
#include "../actor/actor.h"
#include <assert.h>

void pony_gc_send(pony_ctx_t* ctx)
{
  assert(ctx->stack == NULL);
  ctx->trace_object = gc_sendobject;
  ctx->trace_actor = gc_sendactor;

#ifdef USE_TELEMETRY
  ctx->tsc = cpu_tick();
#endif
}

void pony_gc_recv(pony_ctx_t* ctx)
{
  assert(ctx->stack == NULL);
  ctx->trace_object = gc_recvobject;
  ctx->trace_actor = gc_recvactor;

#ifdef USE_TELEMETRY
  ctx->tsc = cpu_tick();
#endif
}

void pony_gc_mark(pony_ctx_t* ctx)
{
  assert(ctx->stack == NULL);
  ctx->trace_object = gc_markobject;
  ctx->trace_actor = gc_markactor;
}

void pony_gc_acquire(pony_ctx_t *ctx)
{
  assert(ctx->stack == NULL);
  ctx->trace_object = gc_acquireobject;
  ctx->trace_actor = gc_acquireactor;
}

void pony_acquire_done(pony_ctx_t *ctx)
{
  gc_handlestack(ctx);
  gc_sendacquire(ctx);
  gc_done(actor_gc(ctx->current));
}

void pony_send_done(pony_ctx_t* ctx)
{
  gc_handlestack(ctx);
  gc_sendacquire(ctx);
  gc_done(actor_gc(ctx->current));

#ifdef USE_TELEMETRY
  ctx->time_in_send_scan += (cpu_tick() - ctx->tsc);
#endif
}

void pony_recv_done(pony_ctx_t* ctx)
{
  gc_handlestack(ctx);
  gc_done(actor_gc(ctx->current));

#ifdef USE_TELEMETRY
  ctx->time_in_recv_scan += (cpu_tick() - ctx->tsc);
#endif
}

void pony_trace(pony_ctx_t* ctx, void* p)
{
  ctx->trace_object(ctx, p, NULL);
}

void pony_traceactor(pony_ctx_t* ctx, pony_actor_t* p)
{
  if (!p) {
    return;
  }
  ctx->trace_actor(ctx, p);
}

void pony_traceobject(pony_ctx_t* ctx, void* p, pony_trace_fn f)
{
  if (!p) {
    return;
  }
  ctx->trace_object(ctx, p, f);
}

void pony_traceunknown(pony_ctx_t* ctx, void* p)
{
  pony_type_t* type = *(pony_type_t**)p;

  if(type->dispatch != NULL)
  {
    ctx->trace_actor(ctx, (pony_actor_t*)p);
  } else {
    ctx->trace_object(ctx, p, type->trace);
  }
}

void pony_trace_tag_or_actor(pony_ctx_t* ctx, void* p)
{
  pony_type_t* type = *(pony_type_t**)p;

  if(type->dispatch != NULL)
  {
    ctx->trace_actor(ctx, (pony_actor_t*)p);
  } else {
    ctx->trace_object(ctx, p, NULL);
  }
}
