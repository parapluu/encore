#define _XOPEN_SOURCE 800
#include <ucontext.h>

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include <assert.h>
#include <pthread.h>
#include <pony/pony.h>

#include "encore.h"
#include "future.h"
#include "../src/actor/messageq.h"

#define BLOCK    pthread_mutex_lock(&fut->lock);
#define UNBLOCK  pthread_mutex_unlock(&fut->lock);
#define perr(m)  // fprintf(stderr, "%s\n", m);

extern pony_actor_t *actor_current();

typedef struct chain_entry chain_entry_t;
typedef struct actor_entry actor_entry_t;
typedef struct closure_entry closure_entry_t;
typedef struct message_entry message_entry_t;

// Terminology:
// Producer -- the actor responsible for fulfilling a future
// Consumer -- an non-producer actor using a future

typedef enum
{
  // A closure that should be run by the producer
  DETACHED_CLOSURE,
  // A closure that should be run ty the consumer
  ATTACHED_CLOSURE,
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

struct future
{
  encore_arg_t      value;
  pony_type_t    *type;
  bool            fulfilled;
  bool            gc_recv;
  // Stupid limitation for now
  actor_entry_t   responsibilities[16];
  int             no_responsibilities;
  // Lock-based for now
  pthread_mutex_t lock;
  future_t *parent;
  closure_entry_t *children;
};

static inline void future_gc_send_value(future_t *fut);
static inline void future_gc_recv_value(future_t *fut);

// TODO call this finalizer in startgc block in actor_run
static void final_fn(void *p)
{
  future_gc_recv_value(p);
}

pony_type_t future_type = {
  ID_FUTURE,
  sizeof(struct future),
  future_trace,
  NULL,
  NULL,
  NULL,
  NULL
};

pony_type_t *future_get_type(future_t *fut){
  return fut->type;
}

static void trace_closure_entry(void *p)
{
  pony_trace(p);
  closure_entry_t *c = (closure_entry_t*)p;
  pony_traceactor(c->actor);
  pony_traceobject(c->future, future_trace);
  pony_traceobject(c->closure, closure_trace);
}

void future_trace(void* p)
{
  /// Currently this does not block -- as futures grow monotonically,
  /// concurrent access should not be a problem.
  perr("future_trace");

  future_t* fut = (future_t*)p;

  if (future_fulfilled(fut)) {
    if (fut->type == ENCORE_ACTIVE) {
      pony_traceactor(fut->value.p);
    } else if (fut->type != ENCORE_PRIMITIVE) {
      pony_traceobject(fut->value.p, fut->type->trace);
    }
  }

  // TODO before we deal with deadlocking and closure with attached semantics
  // any actor in responsibilities also exists in children, so only trace children
  // for (int i = 0; i < fut->no_responsibilities; ++i) {
  //   pony_traceactor(fut->responsibilities[i].message.actor);
  // }

  // TODO closure now has detached semantics, deadlock is not resolved.
  // if (fut->parent) pony_traceobject(fut->parent, future_trace);
}

// ===============================================================
// Create, inspect and fulfil
// ===============================================================
future_t *future_mk(pony_type_t *type)
{
  perr("future_mk");

  future_t *fut = encore_alloc(sizeof(future_t));
  *fut = (future_t) { .type = type };

  pthread_mutex_init(&fut->lock, NULL);
  return fut;
}

encore_arg_t run_closure(closure_t *c, encore_arg_t value)
{
  return closure_call(c, (value_t[1]) { value });
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

encore_arg_t future_read_value(future_t *fut)
{
  perr("future_read_value");
  BLOCK;
  encore_arg_t v = fut->value;
  UNBLOCK;
  return v;
}

void future_fulfil(future_t *fut, encore_arg_t value)
{
  perr("future_fulfil");
  assert(fut->fulfilled == false);

  BLOCK;
  fut->value = value;
  fut->fulfilled = true;

  future_gc_send_value(fut);

  for (int i = 0; i < fut->no_responsibilities; ++i) {
    actor_entry_t e = fut->responsibilities[i];
    switch (e.type)
    {
      case BLOCKED_MESSAGE:
        {
          perr("Unblocking");
          actor_set_resume((encore_actor_t*)e.message.actor);
          pony_schedule_first(e.message.actor);
          break;
        }
        // Current design: send closure back to origin to execute (deadlock-prone)
        // Intended design: see https://github.com/parapluu/mylittlepony/wiki/Futures
      case ATTACHED_CLOSURE:
        {
          encore_arg_t argv[3] = { { .p = e.closure.closure }, value, { .p = e.closure.future } };
          // pony_sendv(e.closure.actor, FUT_MSG_RUN_CLOSURE, 3, argv);
          break;
        }
        // Design 1: current thread executes closures (racy)
      case DETACHED_CLOSURE:
        {
          // value_t result = closure_call(e.closure.closure, (value_t[1]) { value });
          // future_fulfil(e.closure.future, result);
          break;
        }
    }
  }

  closure_entry_t *current = fut->children;
  while(current) {
    encore_arg_t result = run_closure(current->closure, value);
    future_fulfil(current->future, result);

    pony_gc_recv();
    trace_closure_entry(current);
    pony_recv_done();

    current = current->next;
  }

  UNBLOCK;
}

// ===============================================================
// Means for actors to get, block and chain
// ===============================================================
encore_arg_t future_get_actor(future_t *fut)
{
  if (!fut->fulfilled) {
    future_block_actor(fut);
  }

  if (!fut->gc_recv) {
    fut->gc_recv = true;
    future_gc_recv_value(fut);
  }

  return fut->value;

  // /// TODO: in this case, we need to run future_gc_recv_value() too!
  // return run_closure(fut->closure, future_read_value(fut->parent), fut);
}

future_t  *future_chain_actor(future_t *fut, future_t* r, closure_t *c)
{
  perr("future_chain_actor");
  BLOCK;

  if (fut->fulfilled) {
    value_t result = run_closure(c, fut->value);
    future_fulfil(r, result);
    UNBLOCK;
    return r;
  }

  closure_entry_t *entry = encore_alloc(sizeof *entry);
  entry->actor = actor_current();
  entry->future = r;
  entry->closure = c;
  entry->next = fut->children;

  fut->children = entry;

  pony_gc_send();
  trace_closure_entry(entry);
  pony_send_done();

  UNBLOCK;

  r->parent = fut;

  return r;
}

void future_block_actor(future_t *fut)
{
  perr("future_block_actor");

  pony_actor_t *a = actor_current();
  BLOCK;

  if (fut->fulfilled) {
    UNBLOCK;
    return;
  }

  pony_unschedule(a);
  assert(fut->no_responsibilities < 16);
  fut->responsibilities[fut->no_responsibilities++] = (actor_entry_t) { .type = BLOCKED_MESSAGE, .message = (message_entry_t) { .actor = a } };

  encore_actor_t *actor = (encore_actor_t*) a;

  actor->lock = &fut->lock;
  actor_block(actor);
}

// ===============================================================
// Possibly these functions do not belong in the future library
// ===============================================================
void future_suspend(void)
{
  // FIXME: move context into message
  // TODO: block GC'ing during suspend
  /// actor_suspend(actor_current());
}

// FIXME: better type for this
void future_suspend_resume(void *arg)
{
  ctx_wrapper *d = (ctx_wrapper *) arg;
  ucontext_t* ctx = d->ctx;
  ctx->uc_link = d->uc_link;

  pony_actor_t *actor = actor_current();
  /// actor_set_run_to_completion(actor);

  assert(swapcontext(ctx->uc_link, ctx) == 0);


  /// if (actor_run_to_completion(actor)) {
  ///     reclaim_page(actor);
  /// }
}

void future_await(future_t *fut)
{
  /// actor_await(actor_current(), fut);
}

// FIXME: better type for this
void future_await_resume(void *argv)
{
  ctx_wrapper *d = ((encore_arg_t *)argv)[0].p;
  ucontext_t* ctx = d->ctx;
  ctx->uc_link = d->uc_link;

  future_t *fut = ((encore_arg_t *)argv)[1].p;

  if (future_fulfilled(fut))
  {
    pony_actor_t *actor = actor_current();
    /// actor_set_run_to_completion(actor);

    assert(swapcontext(ctx->uc_link, ctx) == 0);

    /// if (actor_run_to_completion(actor)) {
    ///     reclaim_page(actor);
    /// }
  } else {
    // pony_sendv(actor_current(), FUT_MSG_AWAIT, 2, argv);
  }
}

static inline void future_gc_trace_value(future_t *fut)
{
  if (fut->type == ENCORE_ACTIVE) {
    pony_traceactor(fut->value.p);
  } else if (fut->type != ENCORE_PRIMITIVE) {
    pony_traceobject(fut->value.p, fut->type->trace);
  }
}

static inline void future_gc_send_value(future_t *fut)
{
  pony_gc_send();
  future_gc_trace_value(fut);
  pony_send_done();
}

static inline void future_gc_recv_value(future_t *fut)
{
  pony_gc_recv();
  future_gc_trace_value(fut);
  pony_recv_done();
}
