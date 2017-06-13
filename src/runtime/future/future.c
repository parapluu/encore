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

#define GET 0
#define AWAIT 1
#define perr(m)  // fprintf(stderr, "%s\n", m);

static void future_finalizer(future_t *fut);
static inline void future_gc_send_value(pony_ctx_t *ctx, future_t *fut);
static inline void future_gc_recv_value(pony_ctx_t *ctx, future_t *fut);

extern void encore_future_gc_acquireactor(pony_ctx_t* ctx, pony_actor_t* actor);
extern void encore_future_gc_acquireobject(pony_ctx_t* ctx, void* p,
    pony_type_t *t, int mutability);
static void encore_gc_acquire(pony_ctx_t* ctx)
{
  assert(ctx->stack == NULL);
  ctx->trace_object = encore_future_gc_acquireobject;
  ctx->trace_actor = encore_future_gc_acquireactor;
}

typedef struct actor_entry actor_entry_t;

// Terminology:
// Producer -- the actor responsible for fulfilling a future
// Consumer -- an non-producer actor using a future


struct closure_entry
{
  pony_actor_t *actor;
  future_t     *future;
  closure_t    *closure;
  closure_entry_t *next;
};

typedef struct actor_list {
  encore_actor_t *actor;
  ucontext_t *uctx;
  struct actor_list *next;
} actor_list;

static inline void future_block_actor(pony_ctx_t **ctx, future_t *fut);
static void future_finalizer(future_t *fut);
static inline void future_chain(pony_ctx_t **ctx, future_t *fut,
closure_t *c, future_t *r);
static inline void future_gc_send_value(pony_ctx_t *ctx, future_t *fut);
static inline void future_gc_recv_value(pony_ctx_t *ctx, future_t *fut);
static inline void chain_treiber_stack_push(closure_entry_t **head, closure_entry_t *newhead);
static inline closure_entry_t * chain_treiber_stack_pop(closure_entry_t **head);

pony_type_t future_type = {
  .id = ID_FUTURE,
  .size = sizeof(struct future),
  .trace = &future_trace
};

pony_type_t *future_get_type(future_t *fut){
  return fut->type;
}

void future_trace(pony_ctx_t *ctx, void* p)
{
  (void) ctx;
  (void) p;
  // TODO before we deal with deadlocking and closure with attached semantics
  // any actor in responsibilities also exists in children, so only trace children
  // for (int i = 0; i < fut->no_responsibilities; ++i) {
  //   encore_trace_actor(fut->responsibilities[i].message.actor);
  // }

  // TODO closure now has detached semantics, deadlock is not resolved.
  // if (fut->parent) encore_trace_object(fut->parent, future_trace);
}


// ===============================================================
// Create, inspect and fulfil
// ===============================================================

static inline closure_entry_t * chain_treiber_stack_pop(closure_entry_t **head) {
  closure_entry_t * newhead;
  closure_entry_t * oldhead = __atomic_load_n(head, __ATOMIC_SEQ_CST); 
  if (oldhead == NULL) {
    return NULL;  
  }
  newhead = oldhead->next;
  while (!__sync_bool_compare_and_swap(head, oldhead, newhead)) {
    oldhead = __atomic_load_n(head, __ATOMIC_SEQ_CST); 
    if (oldhead == NULL) {
         return NULL;  
      }   
      newhead = oldhead->next;
  }

  return oldhead;
}

static inline void chain_treiber_stack_push(closure_entry_t **head, closure_entry_t *newhead) {
  closure_entry_t * oldhead = __atomic_load_n(head, __ATOMIC_SEQ_CST); 

  newhead->next = oldhead;

  while (!__sync_bool_compare_and_swap(head, oldhead, newhead) ) {
    oldhead = __atomic_load_n(head, __ATOMIC_SEQ_CST); 
    newhead->next = oldhead;
  } 
}

static inline encore_arg_t run_closure(pony_ctx_t **ctx, closure_t *c, encore_arg_t value)
{
  return closure_call(ctx, c, (value_t[1]) { value });
}

bool future_fulfilled(future_t *fut)
{
  perr("future_fulfilled");
  bool r;
  r = __atomic_load_n(&fut->fulfilled, __ATOMIC_SEQ_CST);
  return r;
}

static inline void future_gc_trace_value(pony_ctx_t *ctx, future_t *fut)
{
  assert(fut);
  if (fut->type == ENCORE_ACTIVE) {
    encore_trace_actor(ctx, fut->value.p);
  } else if (fut->type != ENCORE_PRIMITIVE) {
    encore_trace_object(ctx, fut->value.p, fut->type->trace);
  }
}

static inline void future_finalizer(future_t *fut)
{
  pony_ctx_t* cctx = pony_ctx();
  future_gc_recv_value(cctx, fut);
  ENC_DTRACE2(FUTURE_DESTROY, (uintptr_t) cctx, (uintptr_t) fut);
}

static inline void future_gc_send_value(pony_ctx_t *ctx, future_t *fut)
{
  pony_gc_send(ctx);
  future_gc_trace_value(ctx, fut);
  pony_send_done(ctx);
}

static inline void future_gc_recv_value(pony_ctx_t *ctx, future_t *fut)
{
  pony_gc_recv(ctx);
  future_gc_trace_value(ctx, fut);
  // note the asymmetry with send
  ponyint_gc_handlestack(ctx);
}

static inline void acquire_future_value(pony_ctx_t **ctx, future_t *fut)
{
  pony_ctx_t *cctx = *ctx;
  encore_gc_acquire(cctx);
  future_gc_trace_value(cctx, fut);
  pony_acquire_done(cctx);
}

future_t *future_mk(pony_ctx_t **ctx, pony_type_t *type)
{
  pony_ctx_t *cctx = *ctx;
  assert(cctx->current);
  future_t *fut = pony_alloc_final(cctx, sizeof(future_t),
          (void *)&future_finalizer);
  *fut = (future_t) { .type = type, .blocking_stack = NULL, .chain_stack = NULL };
  ENC_DTRACE3(FUTURE_CREATE, (uintptr_t) ctx, (uintptr_t) fut, (uintptr_t) type);
  return fut;
} 

void future_chain_forward(pony_ctx_t **ctx, future_t *fut, pony_type_t *type,
        closure_t *c, future_t *r)
{
  ENC_DTRACE3(FUTURE_CHAINING, (uintptr_t) *ctx, (uintptr_t) fut, (uintptr_t) type);
  (void)type;
  perr("future_chain_actor");
  if (__atomic_load_n(&(fut->fulfilled), __ATOMIC_SEQ_CST)) {
    acquire_future_value(ctx, fut);
    value_t result = run_closure(ctx, c, fut->value);
    future_fulfil(ctx, fut, result);
    return;
  }
  future_chain(ctx, fut, c, r);
  return;
}

future_t* future_chain_actor(pony_ctx_t **ctx, future_t *fut, pony_type_t *type,
        closure_t *c)
{
  ENC_DTRACE3(FUTURE_CHAINING, (uintptr_t) *ctx, (uintptr_t) fut, (uintptr_t) type);
  future_t *r = future_mk(ctx, type);
  perr("future_chain_actor");
  if (__atomic_load_n(&(fut->fulfilled), __ATOMIC_SEQ_CST)) {
    acquire_future_value(ctx, fut);
    value_t result = run_closure(ctx, c, fut->value);
    future_fulfil(ctx, r, result);
    return r;
  }
  future_chain(ctx, fut, c, r);
  return r;  
}

static inline void future_chain(pony_ctx_t **ctx, future_t *fut, closure_t *c, future_t *r)
{
  pony_ctx_t* cctx = *ctx;
  closure_entry_t *entry = POOL_ALLOC(closure_entry_t);
  *entry = (closure_entry_t) { .actor = (cctx)->current, .future = r, .closure = c }; 
  chain_treiber_stack_push(&fut->chain_stack, entry);
  if (__atomic_load_n(&(fut->fulfilled), __ATOMIC_SEQ_CST)) {
    future_discharge(ctx, fut);
  }
}


// Similar to `future_chain_actor` except that it returns void, avoiding the
// creation of a new future. This is used in the ParTs library and is an
// optimisation over the `future_chain_actor`.
void future_register_callback(pony_ctx_t **ctx,
                              future_t *fut,
                              closure_t *c)
{
  ENC_DTRACE2(FUTURE_REGISTER_CALLBACK, (uintptr_t) *ctx, (uintptr_t) fut);
  perr("future_chain_actor");
  if (__atomic_load_n(&(fut->fulfilled), __ATOMIC_SEQ_CST)) {
    acquire_future_value(ctx, fut);
    // the closure is in charge of fulfilling the promise that it contains.
    // if this is not the case, a deadlock situation may happen.
    run_closure(ctx, c, fut->value);
    return ;
  }
  pony_ctx_t* cctx = *ctx;
  closure_entry_t *entry = POOL_ALLOC(closure_entry_t);
  *entry = (closure_entry_t){ .actor = (cctx)->current, .future = NULL, .closure = c };
  chain_treiber_stack_push(&fut->chain_stack, entry);
  if (__atomic_load_n(&(fut->fulfilled), __ATOMIC_SEQ_CST)) {
    future_discharge(ctx, fut);
  }
}

void future_await(pony_ctx_t **ctx, future_t *fut)
{
  if (__atomic_load_n(&(fut->fulfilled), __ATOMIC_SEQ_CST)) {
    return;
  }
  ucontext_t uctx;
  future_tnode_info_t * pony_node = POOL_ALLOC(future_tnode_info_t);
  *pony_node = (future_tnode_info_t) { .fut = (void*)fut, .awaited_uctx = &uctx, .isget = false, .fclass = FUTURE };
  actor_await(ctx, &uctx, (void *)pony_node);
}

encore_arg_t future_get_actor(pony_ctx_t **ctx, future_t *fut)
{
  if (!__atomic_load_n(&(fut->fulfilled), __ATOMIC_SEQ_CST)) {
    ENC_DTRACE2(FUTURE_BLOCK, (uintptr_t) *ctx, (uintptr_t) fut);
    future_block_actor(ctx, fut);
    ENC_DTRACE2(FUTURE_UNBLOCK, (uintptr_t) *ctx, (uintptr_t) fut);
  }
  ENC_DTRACE2(FUTURE_GET, (uintptr_t) *ctx, (uintptr_t) fut);
  return fut->value;
}

static inline void future_block_actor(pony_ctx_t **ctx, future_t *fut)
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
  *pony_node = (future_tnode_info_t) { .fut = (void*)fut, .awaited_uctx = NULL, .isget = true, .fclass = FUTURE };
  actor_block(ctx, actor, (void *)pony_node);          
}

void future_fulfil(pony_ctx_t **ctx, future_t *fut, encore_arg_t value) 
{
  assert(fut->fulfilled == false);
  ENC_DTRACE2(FUTURE_FULFIL_START, (uintptr_t) *ctx, (uintptr_t) fut);
  fut->value = value;
  __atomic_store_n(&(fut->fulfilled), true, __ATOMIC_SEQ_CST);
  future_gc_send_value(*ctx, fut);
  future_discharge(ctx, fut);
  ENC_DTRACE2(FUTURE_FULFIL_END, (uintptr_t) *ctx, (uintptr_t) fut);
}

void future_discharge(pony_ctx_t **ctx, future_t *fut)
{
  future_tnode_t * a = NULL;
  pony_ctx_t * cctx = * ctx;
  encore_actor_t * ea;

  a = treiber_stack_pop(&fut->blocking_stack);
  while (a!=NULL) {
      if (a->isget) {
        ea = (encore_actor_t *)a->actor; 
        perr("Unblocking");
        actor_set_resume(ea);
        pony_schedule(cctx, a->actor);
      } else { 
        pony_sendp(cctx, a->actor, _ENC__MSG_RESUME_AWAIT, a->awaited_uctx);
      }  

      POOL_FREE(future_tnode_t, a);
      a = treiber_stack_pop(&fut->blocking_stack);
  }

  closure_entry_t *current = chain_treiber_stack_pop(&fut->chain_stack);
  while(current) {
    encore_arg_t result = run_closure(ctx, current->closure, fut->value);
    if (current->future) {
      future_fulfil(ctx, current->future, result);
    }

    POOL_FREE(closure_entry_t, current);
    current = chain_treiber_stack_pop(&fut->chain_stack);
  } 

}

/**

Treiber stack implementation

**/

future_tnode_t * treiber_stack_pop(future_tnode_t **head) 
{
  future_tnode_t * newhead;
  future_tnode_t * oldhead = __atomic_load_n(head, __ATOMIC_SEQ_CST); 
  if (oldhead == NULL) {
    return NULL;  
  }
  newhead = oldhead->next;
  while (!__sync_bool_compare_and_swap(head, oldhead, newhead)) {
    oldhead = __atomic_load_n(head, __ATOMIC_SEQ_CST); 
    if (oldhead == NULL) {
         return NULL;  
      }   
      newhead = oldhead->next;
  }

  return oldhead;
}

void treiber_stack_push(future_tnode_t **head, pony_actor_t* a, ucontext_t *awaited_uctx, int isget) 
{
  future_tnode_t * newhead = POOL_ALLOC(future_tnode_t);  //encore_alloc(cctx, sizeof(future_tnode_t));
  future_tnode_t * oldhead = __atomic_load_n(head, __ATOMIC_SEQ_CST); 

  *newhead = (future_tnode_t) { .actor = a, .awaited_uctx = awaited_uctx, .isget = isget, .next = oldhead };

  while (!__sync_bool_compare_and_swap(head, oldhead, newhead) ) {
    oldhead = __atomic_load_n(head, __ATOMIC_SEQ_CST); 
    newhead->next = oldhead;
  } 
}


