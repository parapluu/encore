#define __STDC_FORMAT_MACROS 1
#include "pony/pony.h"
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

#define DEBUG_PRINT 0

typedef struct future_actor_fields {
  bool fulfilled;
  bool has_blocking;
  void *value;
  set_t *blocked;
  set_t *chained;
  set_t *awaiting;
} future_actor_fields;

typedef struct chained_entry {
  pony_actor_t *actor;
  struct closure *closure;
} chained_entry;

typedef struct blocked_entry {
  pony_actor_t *actor;
  resumable_t *context;
} blocked_entry;

static void future_actor_trace();

pony_actor_type_t future_actor_type =
  {
    1,
    {sizeof(future_actor_fields), future_actor_trace, NULL, NULL},
    future_actor_message_type,
    future_actor_dispatch
  };

// FIXME -- 2nd arg in chain should be a closure, not an actor!
static pony_msg_t m_chain = {2, {PONY_ACTOR, PONY_NONE}};
// FIXME -- 2nd arg should be Ctx defined in context.h
static pony_msg_t m_await = {2, {PONY_ACTOR, PONY_NONE}};
// FIXME -- 2nd arg should be Ctx defined in context.h
static pony_msg_t m_block = {2, {PONY_ACTOR, PONY_NONE}};
// FIXME -- 2nd arg in chain should be any kind of Encore value
static pony_msg_t m_fulfil = {0, {PONY_NONE}}; //XXX We can only store passive values in futures!
// FIXME -- arg should be Ctx defined in context.h
static pony_msg_t m_resume_get = {1, {PONY_NONE} };

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
    *(void**) this = fields;
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

static set_t *getBlocked(future_actor_fields *fields) {
  if (fields->blocked == NULL) {
    fields->blocked = mk_set();
  }
  return fields->blocked;
}

static set_t *getChained(future_actor_fields *fields) {
  if (fields->chained == NULL) {
    fields->chained = mk_set();
  }
  return fields->chained;
}

static set_t *getAwaiting(future_actor_fields *fields) {
  if (fields->awaiting == NULL) {
    fields->awaiting = mk_set();
  }
  return fields->awaiting;
}

static void trace_resume(blocked_entry *entry) {
  pony_actor_t *target = entry->actor;
  pony_traceactor(target);
}

static void trace_chain(chained_entry *entry) {
  pony_actor_t *target = entry->actor;
  struct closure *closure = entry->closure;
  pony_traceactor(target);
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

  pony_continuation(target, FUT_MSG_RESUME, 1, argv);
  pony_schedule(target);
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
      set_t *chained = getChained(p);
      chained_entry *new_entry = pony_alloc(sizeof(chained_entry));
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

      set_t *blocked = getBlocked(p);
      blocked_entry *new_entry = pony_alloc(sizeof(blocked_entry));
      new_entry->actor = argv[0].p;
      new_entry->context = (resumable_t*) argv[1].p;
      set_add(blocked, new_entry);
      break;
    }
  case FUT_MSG_AWAIT:
    {
      if (DEBUG_PRINT) fprintf(stderr, "[%p]\t(%p, %p) await ---> %p \n", pthread_self(), argv[0].p, argv[1].p, this);
      set_t *awaiting = getAwaiting(p);
      blocked_entry *new_entry = pony_alloc(sizeof(blocked_entry));
      new_entry->actor = argv[0].p;
      new_entry->context = (resumable_t*) argv[1].p;
      set_add(awaiting, new_entry);
      break;
    }
  case FUT_MSG_FULFIL:
    {
      void *value;
      bool fulfilled;
      fulfilled = future_actor_get_value_and_fulfillment(this, &value);

      if (DEBUG_PRINT) fprintf(stderr, "[%p]\tfulfil ---> %p [value=%p,status=%d]\n", pthread_self(), this, value, fulfilled);

      set_t *chained = getChained(p);
      set_forall(chained, (void *((*)(void *, void*))) run_chain, (void*) value);

      set_t *awaiting = getAwaiting(p);
      set_forall(awaiting, (void *((*)(void *, void*))) resume_from_await, NULL);

      set_t *blocked = getBlocked(p);
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

future_actor_fields *get_fields(pony_actor_t *this) {
  return *(future_actor_fields **)this;
}

void future_actor_set_value(pony_actor_t* this, void *value) {
  future_actor_fields *state = get_fields(this);
  state->value = value;
  bool TRUE = true;
  __atomic_store(&state->value, &value, __ATOMIC_SEQ_CST); //TODO: relax from sc
  __atomic_store(&state->fulfilled, &TRUE, __ATOMIC_SEQ_CST); //TODO: relax from sc
}
bool future_actor_get_value_and_fulfillment(pony_actor_t* this, void **value) {
  future_actor_fields *state = get_fields(this);
  bool fulfilled = __atomic_load_n(&state->fulfilled, __ATOMIC_SEQ_CST); //TODO: relax
  *value = __atomic_load_n(&state->value, __ATOMIC_SEQ_CST);
  return fulfilled;
}
bool future_actor_get_fulfilled(pony_actor_t* this) {
  future_actor_fields *state = get_fields(this);
  return state->fulfilled;
}
void *future_actor_get_value(pony_actor_t* this) {
  future_actor_fields *state = get_fields(this);
  return state->value;
}
void future_actor_set_fulfilled(pony_actor_t* this, bool fulfilled) {
  future_actor_fields *state = get_fields(this);
  __atomic_store(&state->fulfilled, &fulfilled, __ATOMIC_SEQ_CST); //TODO: relax from sc
}
pony_actor_t* future_create() {
  pony_actor_t* fut = pony_create(&future_actor_type);
  future_init(NULL, fut);
  return fut;
}

bool future_has_blocking(pony_actor_t *this) {
  future_actor_fields *state = get_fields(this);
  return __atomic_load_n(&state->has_blocking, __ATOMIC_SEQ_CST); //TODO: relax
}

void future_set_blocking(pony_actor_t *this) {
  future_actor_fields *state = get_fields(this);
  bool TRUE = true;
  __atomic_store_n(&state->has_blocking, &TRUE, __ATOMIC_SEQ_CST); //TODO: relax
}

static void future_actor_trace()
{
  future_actor_fields *fields = pony_get();

  set_forall(fields->chained, (void *((*)(void *, void*))) trace_chain, NULL);
  set_forall(fields->awaiting, (void *((*)(void *, void*))) trace_resume, NULL);
  set_forall(fields->blocked, (void *((*)(void *, void*))) trace_resume, NULL);
}

