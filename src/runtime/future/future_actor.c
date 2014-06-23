#define __STDC_FORMAT_MACROS
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <assert.h>
#include <pthread.h>
#include "future_actor.h"
#include "future.h"
#include "set.h"
#include "closure.h"
#include "actor_def.h"
#include "scheduler.h"

#define DEBUG_PRINT 1

typedef struct future_actor_fields {
  volatile bool fulfilled;
  volatile void *value;
  Set blocked;
  Set chained;
  Set awaiting;
} future_actor_fields;

typedef struct chained_entry {
  pony_actor_t *actor;
  struct closure *closure;
} chained_entry;

typedef struct blocked_entry {
  pony_actor_t *actor;
  resumable_t *context;
} blocked_entry;

static void future_actor_trace(void* p);

pony_actor_type_t future_actor_type =
  {
    1,
    {future_actor_trace, sizeof(future_actor_fields), PONY_ACTOR},
    future_actor_message_type,
    future_actor_dispatch
  };

// FIXME -- 2nd arg in chain should be a closure, not an actor!
static pony_msg_t m_chain = {2,
                             {
                               {NULL, 0, PONY_ACTOR},
                               {NULL, 0, PONY_PRIMITIVE}}};
// FIXME -- 2nd arg should be Ctx defined in context.h
static pony_msg_t m_await = {2,
                             {
                               {NULL, 0, PONY_ACTOR},
                               {NULL, 0, PONY_PRIMITIVE}}};
// FIXME -- 2nd arg should be Ctx defined in context.h
static pony_msg_t m_block = {2,
                             {
                               {NULL, 0, PONY_ACTOR},
                               {NULL, 0, PONY_PRIMITIVE}}};
// FIXME -- 2nd arg in chain should be any kind of Encore value
static pony_msg_t m_fulfil = {0, {{NULL, 0, PONY_ACTOR}}};
// FIXME -- arg should be Ctx defined in context.h
static pony_msg_t m_resume_get = {1, {{NULL, 0, PONY_PRIMITIVE}} };

pony_msg_t* future_actor_message_type(uint64_t id) {
  switch (id) {
  case FUT_MSG_CHAIN: return &m_chain;
  case FUT_MSG_BLOCK: return &m_block;
  case FUT_MSG_AWAIT: return &m_await;
  case FUT_MSG_FULFIL: return &m_fulfil;
  case FUT_MSG_RESUME: return &m_resume_get;
  }

  return NULL;
}

static future_actor_fields *future_init(void *p, void *this) {
  if (p == NULL) {
    future_actor_fields *fields = pony_alloc(sizeof (future_actor_fields));
    ((pony_actor_t*) this)->p = fields;
    fields->fulfilled = false;
    fields->value = NULL;
    fields->blocked = NULL;
    fields->chained = NULL;
    fields->awaiting = NULL;
    return fields;
  } else {
    return (future_actor_fields*) p;
  }
}

static Set getBlocked(pony_actor_t* this) {
  future_actor_fields *fields = this->p;
  if (fields->blocked == NULL) {
    fields->blocked = set_new();
  }
  return fields->blocked;
}

static Set getChained(pony_actor_t* this) {
  future_actor_fields *fields = this->p;
  if (fields->chained == NULL) {
    fields->chained = set_new();
  }
  return fields->chained;
}

static Set getAwaiting(pony_actor_t* this) {
  future_actor_fields *fields = this->p;
  if (fields->awaiting == NULL) {
    fields->awaiting = set_new();
  }
  return fields->awaiting;
}

static void trace_resume(blocked_entry *entry) {
  pony_actor_t *target = entry->actor;
  pony_traceactor(&target);
}

static void trace_chain(chained_entry *entry) {
  pony_actor_t *target = entry->actor;
  struct closure *closure = entry->closure;
  pony_traceactor(&target);
  closure_trace(closure);
}

static void resume_from_await(blocked_entry *entry) {
  pony_actor_t *target = entry->actor;
  resumable_t *r = entry->context;
  pony_arg_t argv[1];
  argv[0].p = r;
  if (DEBUG_PRINT) fprintf(stderr, "[%p]\t%p <--- resume (%p)\n", pthread_self(), target, r);

  pony_sendv(target, FUT_MSG_RESUME, 1, argv);
}

static void resume_from_block(blocked_entry *entry) {
  pony_actor_t *target = entry->actor;
  resumable_t *r = entry->context;
  pony_arg_t argv[1];
  argv[0].p = r;
  if (DEBUG_PRINT) fprintf(stderr, "[%p]\t%p <--- resume (%p)\n", pthread_self(), target, r);

  scheduler_add(target, target->thread);
  // pony_actor_unblock(target);
  // XXX: For now, implement futures with await semantics even on blocking
  pony_sendv(target, FUT_MSG_RESUME, 1, argv);
}

static void run_chain(chained_entry *entry, void *value) {
  pony_actor_t *target = entry->actor;
  struct closure *closure = entry->closure;
  pony_arg_t argv[2];
  argv[0].p = closure;
  argv[1].p = value;
  if (DEBUG_PRINT) fprintf(stderr, "[%p]\t%p <--- run closure (%p)\n", pthread_self(), target, closure);
  pony_sendv(target, FUT_MSG_RUN_CLOSURE, 2, argv); // - see https://trello.com/c/kod5Ablj
}

void future_actor_dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv) {
  switch (id) {
  case FUT_MSG_CHAIN:
    {
      if (DEBUG_PRINT) fprintf(stderr, "[%p]\t(%p, %p) chain ---> %p \n", pthread_self(), argv[0].p, argv[1].p, this);
      // TODO: Add the closure argument to an internal list of closures
      // This entry should record the closure and the actor to run it
      Set chained = getChained(this);
      chained_entry *new_entry = calloc(1, sizeof(chained_entry));
      new_entry->actor = argv[0].p;
      new_entry->closure = argv[1].p;
      set_add(chained, new_entry);
      break;
    }
  case FUT_MSG_BLOCK:
    {
      if (DEBUG_PRINT) fprintf(stderr, "[%p]\t(%p, %p) block ---> %p \n", pthread_self(), argv[0].p, argv[1].p, this);

      if (future_fulfilled((future_t*) this)) {
        if (DEBUG_PRINT) fprintf(stderr, "[%p]\tAttempt to block on fulfilled future\n", pthread_self());

        blocked_entry new_entry = { .actor = argv[0].p , .context = argv[1].p };
        resume_from_block(&new_entry);

        break;
      }

      Set blocked = getBlocked(this);
      blocked_entry *new_entry = malloc(sizeof(blocked_entry));
      new_entry->actor = argv[0].p;
      new_entry->context = (resumable_t*) argv[1].p;
      set_add(blocked, new_entry);
      break;
    }
  case FUT_MSG_AWAIT:
    {
      if (DEBUG_PRINT) fprintf(stderr, "[%p]\t(%p, %p) await ---> %p \n", pthread_self(), argv[0].p, argv[1].p, this);
      Set awaiting = getAwaiting(this);
      blocked_entry *new_entry = malloc(sizeof(blocked_entry));
      new_entry->actor = argv[0].p;
      new_entry->context = (resumable_t*) argv[1].p;
      set_add(awaiting, new_entry);
      break;
    }
  case FUT_MSG_FULFIL:
    {
      // Discarding volatile here because it does not matter
      volatile void *value;
      bool fulfilled;
      fulfilled = future_actor_get_value_and_fulfillment(this, &value);

      if (DEBUG_PRINT) fprintf(stderr, "[%p]\tfulfil ---> %p [value=%p,status=%d]\n", pthread_self(), this, value, fulfilled);

      Set chained = getChained(this);
      set_forall(chained, (void *((*)(void *, void*))) run_chain, (void*) value);

      Set awaiting = getAwaiting(this);
      set_forall(awaiting, (void *((*)(void *, void*))) resume_from_await, NULL);

      Set blocked = getBlocked(this);
      set_forall(blocked, (void *((*)(void *, void*))) resume_from_block, NULL);

      break;
    }
  case FUT_MSG_RESUME:
    {
      if (DEBUG_PRINT) fprintf(stderr, "[%p]\t(%p) resume ---> %p \n", pthread_self(), argv[0].p, this);
      // XXX: This behaviour is controlled by the eager/lazy strategy
      // t_resume(argv[0].p);
      break;
    }
  case FUT_MSG_RUN_CLOSURE:
    {
      if (DEBUG_PRINT) fprintf(stderr, "[%p]\tThis point should not be reached!\n", pthread_self());
      assert(false);
      break;
    }
  }
}

void future_actor_set_value(pony_actor_t* this, volatile void *value) {
  future_actor_fields *state = this->p;
  state->value = value;
  __sync_synchronize();
  state->fulfilled = true;
}
volatile bool future_actor_get_value_and_fulfillment(pony_actor_t* this, volatile void **value) {
  future_actor_fields *state = this->p;
  volatile bool fulfilled = state->fulfilled;
  __sync_synchronize();
  *value = state->value;
  return fulfilled;
}
volatile bool future_actor_get_fulfilled(pony_actor_t* this) {
  future_actor_fields *state = this->p;
  return state->fulfilled;
}
volatile void *future_actor_get_value(pony_actor_t* this) {
  future_actor_fields *state = this->p;
  return state->value;
}
void future_actor_set_fulfilled(pony_actor_t* this, volatile bool fulfilled) {
  future_actor_fields *state = this->p;
  state->fulfilled = fulfilled;
}
pony_actor_t* future_create() {
  pony_actor_t* fut = pony_create(&future_actor_type);
  future_init(NULL, fut);
  return fut;
}

static void future_actor_trace(void* p)
{
  future_actor_fields *fields = p;

  set_forall(fields->chained, (void *((*)(void *, void*))) trace_chain, NULL);
  set_forall(fields->awaiting, (void *((*)(void *, void*))) trace_resume, NULL);
  set_forall(fields->blocked, (void *((*)(void *, void*))) trace_resume, NULL);
}

