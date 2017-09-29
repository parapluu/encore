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

pthread_mutexattr_t attr;
#define BLOCK    pthread_mutex_lock(&fut->lock);
#define UNBLOCK  pthread_mutex_unlock(&fut->lock);
#define perr(m)  // fprintf(stderr, "%s\n", m);

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
typedef struct closure_entry closure_entry_t;
typedef struct message_entry message_entry_t;

// Terminology:
// Producer -- the actor responsible for fulfilling a future
// Consumer -- an non-producer actor using a future

typedef enum responsibility_t
{
  // A closure that should be run by the producer
  DETACHED_CLOSURE,
  // A message blocked on this future
  BLOCKED_MESSAGE
} responsibility_t;

struct closure_entry
{
  // The consumer that created closure
  pony_actor_t *actor;
  // The future where the result of the closure should be stored
  future_t     *future;
  // The closure to be run on fulfilment of the future
  closure_t    *closure;

  closure_entry_t *next;

};

struct message_entry
{
  // The consumer that created closure
  pony_actor_t *actor;
  // FIXME: add context
};

struct actor_entry
{
  responsibility_t type;
  union
  {
    closure_entry_t closure;
    message_entry_t message;
  };
};

typedef struct actor_list {
  encore_actor_t *actor;
  ucontext_t *uctx;
  struct actor_list *next;
} actor_list;

struct future
{
  pony_type_t *future_type;
  encore_arg_t      value;
  pony_type_t    *type;
  bool            fulfilled;
  // Stupid limitation for now
  actor_entry_t   responsibilities[16];
  int             no_responsibilities;
  // Lock-based for now
  pthread_mutex_t lock;
  future_t *parent;
  closure_entry_t *children;
  actor_list *awaited_actors;
};

static void future_block_actor(pony_ctx_t **ctx, future_t *fut);
static void future_finalizer(future_t *fut);
static inline void future_gc_send_value(pony_ctx_t *ctx, future_t *fut);
static inline void future_gc_recv_value(pony_ctx_t *ctx, future_t *fut);
static void future_chain(pony_ctx_t **ctx, future_t *fut, pony_type_t *type,
    closure_t *c, future_t *r, bool withForward);

pony_type_t future_type = {
  .id = ID_FUTURE,
  .size = sizeof(struct future),
  .trace = &future_trace,
  .final = (void*)&future_finalizer,
};

pony_type_t *future_get_type(future_t *fut){
  return fut->type;
}

static void trace_closure_entry(pony_ctx_t *ctx, void *p)
{
  assert(p);
  pony_trace(ctx, p);
  closure_entry_t *c = (closure_entry_t*)p;
  encore_trace_actor(ctx, c->actor);
  encore_trace_object(ctx, c->future, &future_trace);
  encore_trace_object(ctx, c->closure, &closure_trace);
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

static inline void future_gc_trace_value(pony_ctx_t *ctx, future_t *fut)
{
  assert(fut);
  if (fut->type == ENCORE_ACTIVE) {
    encore_trace_actor(ctx, fut->value.p);
  } else if (fut->type != ENCORE_PRIMITIVE) {
    encore_trace_object(ctx, fut->value.p, fut->type->trace);
  }
}

// ===============================================================
// Create, inspect and fulfil
// ===============================================================

//
// a future has a lock attached to it. actors may acquire the same lock
// multiple times (re-entrant locks). this does not happen when we work with plain Encore
// however, re-entrant locks are necessary for the ParT runtime library.
//
// Use case:
// By allowing re-entrant locks, one can create a promise that is fulfilled only
// when all futures in a ParT (function `party_promise_await_on_futures`) are
// fulfilled. all futures in a ParT get chained a closure that contains the promise
// to fulfil and the original ParT. this promise gets called only when all
// futures in the ParT have been fulfilled. when that happens, the closure
// fulfils the promise and runs some function on the original ParT.
// in pseudo-code:
//
//  val par = liftf(Fut t) :: Par t
//  var promise = new Promise
//  val clos = \(..., env = [par, promise]) -> do something
//  (forall f in (getFuts par), f ~~> clos)
//  await(promise)
//
//  this same code can be found in `party_promise_await_on_futures`.
//
future_t *future_mk(pony_ctx_t **ctx, pony_type_t *type)
{
  pony_ctx_t *cctx = *ctx;
  assert(cctx->current);

  future_t *fut = pony_alloc_final(cctx, sizeof(future_t));
  *fut = (future_t) { .future_type = &future_type, .type = type };

  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init(&fut->lock, &attr);

  ENC_DTRACE3(FUTURE_CREATE, (uintptr_t) ctx, (uintptr_t) fut, (uintptr_t) type);

  return fut;
}

static inline encore_arg_t run_closure(pony_ctx_t **ctx, closure_t *c, encore_arg_t value)
{
  return closure_call(ctx, c, (value_t[1]) { value });
}

static inline void run_closure_fwd(pony_ctx_t **ctx, closure_t *c, encore_arg_t value)
{
  closure_call(ctx, c, (value_t[1]) { value });
  return;
}

bool future_fulfilled(future_t *fut)
{
  perr("future_fulfilled");
  bool r;
  BLOCK;
  r = fut->fulfilled;
  UNBLOCK;
  return r;
}

void future_fulfil(pony_ctx_t **ctx, future_t *fut, encore_arg_t value)
{
  assert(fut->fulfilled == false);
  ENC_DTRACE2(FUTURE_FULFIL_START, (uintptr_t) *ctx, (uintptr_t) fut);

  BLOCK;
  // Update the modifiable context
  fut->value = value;
  fut->fulfilled = true;

  // Create pointer to a `pony_ctx_t * const` (in practice, PonyRT omits the `const`)
  pony_ctx_t *cctx = *ctx;
  future_gc_send_value(cctx, fut);

  for (int i = 0; i < fut->no_responsibilities; ++i) {
    actor_entry_t e = fut->responsibilities[i];
    switch (e.type) {
      case BLOCKED_MESSAGE:
        perr("Unblocking");
        actor_set_resume((encore_actor_t*)e.message.actor);
        pony_schedule(cctx, e.message.actor);
        break;
      case DETACHED_CLOSURE:
        assert(0);
        exit(-1);
    }
  }

  {
    closure_entry_t *current = fut->children;
    while(current) {
      encore_arg_t result = run_closure(ctx, current->closure, value);
      if (current->future) {
        // This case happens when futures can be chained on.
        // As an optimisation to the ParT library, we do know
        // that certain functions in the ParT do not need to fulfil
        // a future, e.g. the ParT optimised version is called
        // `future_register_callback` and sets `current->future = NULL`
        future_fulfil(ctx, current->future, result);
      }

      cctx = *ctx; // ctx might have been changed
      pony_gc_recv(cctx);
      trace_closure_entry(cctx, current);
      pony_recv_done(cctx);

      current = current->next;
    }
  }
  {
    actor_list *current = fut->awaited_actors;
    while(current) {
      pony_sendp(cctx, (pony_actor_t *)current->actor, _ENC__MSG_RESUME_AWAIT,
          current->uctx);

      pony_gc_recv(cctx);
      pony_trace(cctx, current);
      encore_trace_actor(cctx, (pony_actor_t *)current->actor);
      pony_recv_done(cctx);

      current = current->next;
    }
  }

  UNBLOCK;
  ENC_DTRACE2(FUTURE_FULFIL_END, (uintptr_t) cctx, (uintptr_t) fut);
}

static void acquire_future_value(pony_ctx_t **ctx, future_t *fut)
{
  pony_ctx_t *cctx = *ctx;
  encore_gc_acquire(cctx);
  future_gc_trace_value(cctx, fut);
  pony_acquire_done(cctx);
}

// ===============================================================
// Means for actors to get, block and chain
// ===============================================================
encore_arg_t future_get_actor(pony_ctx_t **ctx, future_t *fut)
{
  if (!fut->fulfilled) {
    ENC_DTRACE2(FUTURE_BLOCK, (uintptr_t) *ctx, (uintptr_t) fut);
    future_block_actor(ctx, fut);
    ENC_DTRACE2(FUTURE_UNBLOCK, (uintptr_t) *ctx, (uintptr_t) fut);
  }

  acquire_future_value(ctx, fut);
  ENC_DTRACE2(FUTURE_GET, (uintptr_t) *ctx, (uintptr_t) fut);

  return fut->value;
}

future_t *future_chain_actor(pony_ctx_t **ctx, future_t *fut, pony_type_t *type,
        closure_t *c)
{
  ENC_DTRACE3(FUTURE_CHAINING, (uintptr_t) *ctx, (uintptr_t) fut, (uintptr_t) type);
  future_t *r = future_mk(ctx, type);
  future_chain(ctx, fut, type, c, r, false);
  return r;
}

void future_chain_with_fut(pony_ctx_t **ctx, future_t *fut, pony_type_t *type,
        closure_t *c, future_t *r, bool keepFwd)
{
  ENC_DTRACE3(FUTURE_CHAINING, (uintptr_t) *ctx, (uintptr_t) fut, (uintptr_t) type);
  (void)type;
  future_chain(ctx, fut, type, c, r, keepFwd);
  return;
}

static void future_chain(pony_ctx_t **ctx, future_t *fut, pony_type_t *type,
        closure_t *c, future_t *r, bool withForward)
{
  (void)type;
  perr("future_chain_actor");
  BLOCK;

  if (fut->fulfilled) {
    acquire_future_value(ctx, fut);
    if (withForward) {
      run_closure_fwd(ctx, c, fut->value);
    }
    else {
      value_t result = run_closure(ctx, c, fut->value);
      future_fulfil(ctx, r, result);
    }
    UNBLOCK;
    return;
  }

  pony_ctx_t* cctx = *ctx;
  closure_entry_t *entry = encore_alloc(cctx, sizeof *entry);
  entry->actor = (cctx)->current;
  entry->future = r;
  entry->closure = c;
  entry->next = fut->children;
  fut->children = entry;

  pony_gc_send(cctx);
  trace_closure_entry(cctx, entry);
  pony_send_done(cctx);

  UNBLOCK;

  r->parent = fut;
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
  BLOCK;

  if (fut->fulfilled) {
    acquire_future_value(ctx, fut);

    // the closure is in charge of fulfilling the promise that it contains.
    // if this is not the case, a deadlock situation may happen.
    run_closure(ctx, c, fut->value);
    UNBLOCK;
    return ;
  }

  pony_ctx_t* cctx = *ctx;
  closure_entry_t *entry = encore_alloc(cctx, sizeof *entry);
  entry->actor = (cctx)->current;
  entry->future = NULL;
  entry->closure = c;
  entry->next = fut->children;
  fut->children = entry;

  pony_gc_send(cctx);
  trace_closure_entry(cctx, entry);
  pony_send_done(cctx);

  UNBLOCK;
}


static void future_block_actor(pony_ctx_t **ctx, future_t *fut)
{
  perr("future_block_actor");
  pony_ctx_t* cctx = *ctx;
  pony_actor_t *a = (cctx)->current;
  BLOCK;

  if (fut->fulfilled) {
    UNBLOCK;
    return;
  }

  pony_unschedule(cctx, a);
  assert(fut->no_responsibilities < 16);
  fut->responsibilities[fut->no_responsibilities++] = (actor_entry_t) { .type = BLOCKED_MESSAGE, .message = (message_entry_t) { .actor = a } };

  encore_actor_t *actor = (encore_actor_t*) a;

  assert(actor->lock == NULL);
  actor->lock = &fut->lock;
  actor_block(ctx, actor);
}

// ===============================================================
// Possibly these functions do not belong in the future library
// ===============================================================

void future_await(pony_ctx_t **ctx, future_t *fut)
{
  pony_ctx_t* cctx = *ctx;
  encore_actor_t *actor = (encore_actor_t *)cctx->current;
  BLOCK;
  if (fut->fulfilled) {
    UNBLOCK;
    return;
  }

  ucontext_t uctx;

  actor_list *entry = encore_alloc(cctx, sizeof *entry);
  entry->actor = actor;
  entry->uctx = &uctx;
  entry->next = fut->awaited_actors;
  fut->awaited_actors = entry;

  pony_gc_send(cctx);
  pony_trace(cctx, entry);
  encore_trace_actor(cctx, (pony_actor_t *)entry->actor);
  pony_send_done(cctx);

  assert(actor->lock == NULL);
  actor->lock = &fut->lock;
  actor_await(ctx, &uctx);
}

static void future_finalizer(future_t *fut)
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
