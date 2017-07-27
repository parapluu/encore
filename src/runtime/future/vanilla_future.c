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

/*
      The most minified future API possible in encore.

*/

extern bool pony_system_actor(pony_actor_t *actor);      
static inline void vanilla_future_discharge(pony_ctx_t **ctx, vanilla_future_t * fut);
static void vanilla_future_finalizer(vanilla_future_t *fut);
static inline void vanilla_future_gc_send_value(pony_ctx_t *ctx, vanilla_future_t *fut);
static inline void vanilla_future_gc_recv_value(pony_ctx_t *ctx, vanilla_future_t *fut);
static inline void vanilla_future_block_actor(pony_ctx_t **ctx, vanilla_future_t *fut);
extern void encore_future_gc_acquireactor(pony_ctx_t* ctx, pony_actor_t* actor);
extern void encore_future_gc_acquireobject(pony_ctx_t* ctx, void* p,
    pony_type_t *t, int mutability);

static void vanilla_future_finalizer(vanilla_future_t *fut)
{
  pony_ctx_t* cctx = pony_ctx();
  vanilla_future_gc_recv_value(cctx, fut);
  ENC_DTRACE2(FUTURE_DESTROY, (uintptr_t) cctx, (uintptr_t) fut);
}

pony_type_t *vanilla_future_get_type(vanilla_future_t *fut){
  return fut->type;
}

bool vanilla_future_fulfilled(vanilla_future_t *fut)
{
  perr("future_fulfilled");
  bool r;
  r = __atomic_load_n(&fut->fulfilled, __ATOMIC_SEQ_CST);
  return r;
}

static inline void vanilla_future_gc_trace_value(pony_ctx_t *ctx, vanilla_future_t *fut)
{
  assert(fut);
  if (fut->type == ENCORE_ACTIVE) {
    encore_trace_actor(ctx, fut->value.p);
  } else if (fut->type != ENCORE_PRIMITIVE) {
    encore_trace_object(ctx, fut->value.p, fut->type->trace);
  }
}

static inline void vanilla_future_gc_send_value(pony_ctx_t *ctx, vanilla_future_t *fut)
{
  pony_gc_send(ctx);
  vanilla_future_gc_trace_value(ctx, fut);
  pony_send_done(ctx);
}

static inline void vanilla_future_gc_recv_value(pony_ctx_t *ctx, vanilla_future_t *fut)
{
  pony_gc_recv(ctx);
  vanilla_future_gc_trace_value(ctx, fut);
  ponyint_gc_handlestack(ctx);
}

vanilla_future_t *vanilla_future_mk(pony_ctx_t **ctx, pony_type_t *type)
{
  pony_ctx_t *cctx = *ctx;
  assert(cctx->current);
  vanilla_future_t *fut = pony_alloc_final(cctx, sizeof(future_t),
          (void *)&vanilla_future_finalizer);
  *fut = (vanilla_future_t) { .type = type, .blocking = false };
  ENC_DTRACE3(FUTURE_CREATE, (uintptr_t) ctx, (uintptr_t) fut, (uintptr_t) type);
  return fut;
} 

encore_arg_t vanilla_future_get_actor(pony_ctx_t **ctx, vanilla_future_t *fut)
{
  if (!__atomic_load_n(&(fut->fulfilled), __ATOMIC_SEQ_CST)) {
    ENC_DTRACE2(FUTURE_BLOCK, (uintptr_t) *ctx, (uintptr_t) fut);
    vanilla_future_block_actor(ctx, fut);
    ENC_DTRACE2(FUTURE_UNBLOCK, (uintptr_t) *ctx, (uintptr_t) fut);
  }
  ENC_DTRACE2(FUTURE_GET, (uintptr_t) *ctx, (uintptr_t) fut);
  return fut->value;
}

static inline void vanilla_future_block_actor(pony_ctx_t **ctx, vanilla_future_t *fut)
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
  *pony_node = (future_tnode_info_t) { .fclass = VANILLA_FUTURE, .fut = (void*)fut };
  actor_block(ctx, actor, (void *)pony_node);          
}

void vanilla_future_fulfil(pony_ctx_t **ctx, vanilla_future_t *fut, encore_arg_t value) 
{
  assert(fut->fulfilled == false);
  ENC_DTRACE2(FUTURE_FULFIL_START, (uintptr_t) *ctx, (uintptr_t) fut);
  fut->value = value;
  __atomic_store_n(&(fut->fulfilled), true, __ATOMIC_SEQ_CST);
  vanilla_future_gc_send_value(*ctx, fut);
  vanilla_future_discharge(ctx, fut);
  ENC_DTRACE2(FUTURE_FULFIL_END, (uintptr_t) *ctx, (uintptr_t) fut);
}

static inline void vanilla_future_discharge(pony_ctx_t **ctx, vanilla_future_t *fut) {
  if (__sync_bool_compare_and_swap(&(fut->blocking), true, false)) { 
     perr("Unblocking");
     actor_set_resume((encore_actor_t *)fut->actor);
     pony_schedule(*ctx, fut->actor);
  }   
}

void handle_future(encore_actor_t *actor, pony_ctx_t *futctx, void * pony_node)
{
  if (!pony_system_actor((pony_actor_t*) actor)) {
    if ( pony_node != NULL ) { 
      future_tnode_info_t * pinfo = (future_tnode_info_t *)pony_node;
      if (pinfo->fclass == VANILLA_FUTURE ) {
          vanilla_future_t * fut = (vanilla_future_t *)pinfo->fut;
          fut->actor = (pony_actor_t *)actor;
          __atomic_store_n(&fut->blocking, true, __ATOMIC_SEQ_CST);
          if (__atomic_load_n(&fut->fulfilled, __ATOMIC_SEQ_CST)) {
            vanilla_future_discharge(&futctx, fut);
          }
          POOL_FREE(future_tnode_info_t, pinfo);
      } else if ( pinfo->fclass == POLY_VANILLA_FUTURE ) { 
          poly_vanilla_future_t * fut = (poly_vanilla_future_t *)pinfo->fut;
          treiber_stack_push(&fut->blocking_stack, (pony_actor_t*)actor, NULL, 0);
          POOL_FREE(future_tnode_info_t, (future_tnode_info_t *)pony_node);
          if (__atomic_load_n(&fut->fulfilled, __ATOMIC_SEQ_CST)) {
            poly_vanilla_future_discharge(&futctx, fut);
          }
      } else if ( pinfo->fclass == FUTURE ) { 
          future_t * fut = (future_t *)pinfo->fut;
          treiber_stack_push(&fut->blocking_stack, (pony_actor_t*)actor, pinfo->awaited_uctx, pinfo->isget);
          POOL_FREE(future_tnode_info_t, (future_tnode_info_t *)pony_node);
          if (__atomic_load_n(&fut->fulfilled, __ATOMIC_SEQ_CST)) {
            future_discharge(&futctx, fut);
          }
      }
    } 
  }
}




