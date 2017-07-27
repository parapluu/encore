#define _XOPEN_SOURCE 800
#include <ucontext.h>

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include <assert.h>
#include <pthread.h>
#include <pony.h>
#include <dtrace_encore.h>

#include "encore.h"
#include "future.h"
#include "../libponyrt/actor/messageq.h"
#include "../libponyrt/sched/scheduler.h"

#define perr(m)  // fprintf(stderr, "%s\n", m);

extern void encore_future_gc_acquireactor(pony_ctx_t* ctx, pony_actor_t* actor);
extern void encore_future_gc_acquireobject(pony_ctx_t* ctx, void* p,
    pony_type_t *t, int mutability);

typedef struct actor_entry actor_entry_t;

// Terminology:
// Producer -- the actor responsible for fulfilling a future
// Consumer -- an non-producer actor using a future

static inline void poly_vanilla_future_block_actor(pony_ctx_t **ctx, poly_vanilla_future_t *fut);
static inline void poly_vanilla_future_finalizer(poly_vanilla_future_t *fut);
static inline void poly_vanilla_future_gc_send_value(pony_ctx_t *ctx, poly_vanilla_future_t *fut);
static inline void poly_vanilla_future_gc_recv_value(pony_ctx_t *ctx, poly_vanilla_future_t *fut);


// ===============================================================
// Create, inspect and fulfil
// ===============================================================


static inline void poly_vanilla_future_gc_trace_value(pony_ctx_t *ctx, poly_vanilla_future_t *fut)
{
  assert(fut);
  if (fut->type == ENCORE_ACTIVE) {
    encore_trace_actor(ctx, fut->value.p);
  } else if (fut->type != ENCORE_PRIMITIVE) {
    encore_trace_object(ctx, fut->value.p, fut->type->trace);
  }
}

static inline void poly_vanilla_future_finalizer(poly_vanilla_future_t *fut)
{
  pony_ctx_t* cctx = pony_ctx();
  poly_vanilla_future_gc_recv_value(cctx, fut);
  ENC_DTRACE2(FUTURE_DESTROY, (uintptr_t) cctx, (uintptr_t) fut);
}

static inline void poly_vanilla_future_gc_send_value(pony_ctx_t *ctx, poly_vanilla_future_t *fut)
{
  pony_gc_send(ctx);
  poly_vanilla_future_gc_trace_value(ctx, fut);
  pony_send_done(ctx);
}

static inline void poly_vanilla_future_gc_recv_value(pony_ctx_t *ctx, poly_vanilla_future_t *fut)
{
  pony_gc_recv(ctx);
  poly_vanilla_future_gc_trace_value(ctx, fut);
  // note the asymmetry with send
  ponyint_gc_handlestack(ctx);
}

/**

poly future methods

**/

poly_vanilla_future_t *poly_vanilla_future_mk(pony_ctx_t **ctx, pony_type_t *type)
{
  pony_ctx_t *cctx = *ctx;
  assert(cctx->current);
  poly_vanilla_future_t *fut = pony_alloc_final(cctx, sizeof(poly_vanilla_future_t),
          (void *)&poly_vanilla_future_finalizer);
  *fut = (poly_vanilla_future_t) { .type = type, .blocking_stack = NULL };
  ENC_DTRACE3(FUTURE_CREATE, (uintptr_t) ctx, (uintptr_t) fut, (uintptr_t) type);
  return fut;
} 

encore_arg_t poly_vanilla_future_get_actor(pony_ctx_t **ctx, poly_vanilla_future_t *fut)
{
  if (!__atomic_load_n(&(fut->fulfilled), __ATOMIC_SEQ_CST)) {
    ENC_DTRACE2(FUTURE_BLOCK, (uintptr_t) *ctx, (uintptr_t) fut);
    poly_vanilla_future_block_actor(ctx, fut);
    ENC_DTRACE2(FUTURE_UNBLOCK, (uintptr_t) *ctx, (uintptr_t) fut);
  }
  ENC_DTRACE2(FUTURE_GET, (uintptr_t) *ctx, (uintptr_t) fut);
  return fut->value;
}

static inline void poly_vanilla_future_block_actor(pony_ctx_t **ctx, poly_vanilla_future_t *fut)
{
  perr("future_block_actor");
  if (__atomic_load_n(&(fut->fulfilled), __ATOMIC_SEQ_CST)) {
    return;
  }
  pony_ctx_t* cctx = *ctx;
  pony_actor_t *a = cctx->current;
  pony_unschedule(cctx, a);
  encore_actor_t *actor = (encore_actor_t*) a;
  future_tnode_info_t * pony_node = POOL_ALLOC(future_tnode_info_t);
  *pony_node = (future_tnode_info_t) { .fut = (void*)fut, .fclass = POLY_VANILLA_FUTURE };
  actor_block(ctx, actor, (void *)pony_node);          
}

void poly_vanilla_future_fulfil(pony_ctx_t **ctx, poly_vanilla_future_t *fut, encore_arg_t value) 
{
  assert(fut->fulfilled == false);
  ENC_DTRACE2(FUTURE_FULFIL_START, (uintptr_t) *ctx, (uintptr_t) fut);
  fut->value = value;
  __atomic_store_n(&(fut->fulfilled), true, __ATOMIC_SEQ_CST);
  poly_vanilla_future_gc_send_value(*ctx, fut);
  poly_vanilla_future_discharge(ctx, fut);
  ENC_DTRACE2(FUTURE_FULFIL_END, (uintptr_t) *ctx, (uintptr_t) fut);
}

void poly_vanilla_future_discharge(pony_ctx_t **ctx, poly_vanilla_future_t *fut) {
  future_tnode_t * a = NULL;
  pony_ctx_t * cctx = * ctx;
  encore_actor_t * ea;
  a = treiber_stack_pop(&fut->blocking_stack);
  while (a!=NULL) {
      ea = (encore_actor_t *)a->actor; 
      perr("Unblocking");
      actor_set_resume(ea);
      pony_schedule(cctx, a->actor);
      POOL_FREE(future_tnode_t, a);
      a = treiber_stack_pop(&fut->blocking_stack);
  }
}

