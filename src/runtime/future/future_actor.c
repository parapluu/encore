#define __STDC_FORMAT_MACROS
#include <pony/pony.h>
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
  Set blocked;
  Set chained;
  Set yielded;
} future_actor_fields;

typedef struct chained_entry {
  pony_actor_t *actor;
  struct closure *closure;
} chained_entry;

typedef struct blocked_entry {
  pony_actor_t *actor;
  stacklet_t *context;
} blocked_entry;

pony_actor_type_t future_actor_type =
{
  1,
  {NULL, sizeof(future_actor_fields), PONY_ACTOR},
  future_actor_message_type,
  future_actor_dispatch
};


// FIXME -- 2nd arg in chain should be a closure, not an actor!
static pony_msg_t m_chain = {2,
			     {
			       {NULL, 0, PONY_ACTOR},
			       {NULL, 0, PONY_ACTOR}}};
// FIXME -- 2nd arg should be Ctx defined in context.h
static pony_msg_t m_yield = {2,
			     {
			       {NULL, 0, PONY_ACTOR},
			       {NULL, 0, PONY_ACTOR}}};
// FIXME -- 2nd arg should be Ctx defined in context.h
static pony_msg_t m_block = {2,
			     {
			       {NULL, 0, PONY_ACTOR},
			       {NULL, 0, PONY_ACTOR}}};
// FIXME -- 2nd arg in chain should be any kind of Encore value
static pony_msg_t m_fulfil = {0, {{NULL, 0, PONY_ACTOR}}};
// FIXME -- arg should be Ctx defined in context.h
static pony_msg_t m_resume_get = {1, {{NULL, 0, PONY_ACTOR}} };

pony_msg_t* future_actor_message_type(uint64_t id) {
  switch (id) {
  case FUT_MSG_CHAIN: return &m_chain;
  case FUT_MSG_BLOCK: return &m_block;
  case FUT_MSG_YIELD: return &m_yield;
  case FUT_MSG_FULFIL: return &m_fulfil;
  case FUT_MSG_RESUME: return &m_resume_get;
  }

  return NULL;
}

static void init(pony_actor_t* this) {
  if (this->p) return;
  pony_set(pony_alloc(sizeof (future_actor_fields)));
  // FIXME: are the following inits really necessary? (is memory nulled from start?)
  future_actor_fields *fields = this->p;
  fields->blocked = NULL;
  fields->chained = NULL;
  fields->yielded = NULL;
}

static Set getBlocked(pony_actor_t* this) {
  assert(this);
  future_actor_fields *fields = NULL;
  do {
    fields = __sync_fetch_and_add(&(this->p), 0);
  } while (fields == NULL);

  if (fields->blocked == NULL) {
    fields->blocked = set_new();
  }
  return fields->blocked;
}

static Set getChained(pony_actor_t* this) {
  assert(this);
  future_actor_fields *fields = NULL;
  do {
    fields = __sync_fetch_and_add(&(this->p), 0);
  } while (fields == NULL);

  if (fields->chained == NULL) {
    fields->chained = set_new();
  }
  return fields->chained;
}

static Set getYielded(pony_actor_t* this) {
  assert(this);
  future_actor_fields *fields = NULL;
  do {
    fields = __sync_fetch_and_add(&(this->p), 0);
  } while (fields == NULL);

  if (fields->yielded == NULL) {
    fields->yielded = set_new();
  }
  return fields->yielded;
}

// XXX
extern void pony_actor_unblock(pony_actor_t *actor);

static void resume(blocked_entry *entry) {
  pony_actor_t *target = entry->actor;
  stacklet_t *stacklet = entry->context;
  pony_arg_t argv[1];
  argv[0].p = stacklet;
  if (DEBUG_PRINT) fprintf(stderr, "[%p]\t%p <--- resume (%p)\n", pthread_self(), target, stacklet);

  pony_sendv(target, FUT_MSG_RESUME, 1, argv);
  pony_actor_unblock(target);
}

static void run_chain(chained_entry *entry, void *value) {
  struct closure *closure = entry->closure;
  pony_actor_t *target = entry->actor;
  if (target != NULL) {
    pony_arg_t argv[1];
    argv[0].p = closure;
    argv[1].p = value;
    if (DEBUG_PRINT) fprintf(stderr, "[%p]\t%p <--- run closure (%p)\n", pthread_self(), target, closure);
    pony_sendv(target, FUT_MSG_RUN_CLOSURE, 2, argv); // - see https://trello.com/c/kod5Ablj
  } else {
    union value arg[] = {ptr_to_val(value)};
    closure_call(closure, arg);
  }
}

void future_actor_dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv) {
  init(this);

  switch (id) {
  case FUT_MSG_CHAIN:
    {
      // TODO: Add the closure argument to an internal list of closures
      // This entry should record the closure and the actor to run it
      if (DEBUG_PRINT) fprintf(stderr, "[%p]\tChaining on a future\n", pthread_self());
      Set chained = getChained(this);
      chained_entry *new_entry = pony_alloc(sizeof(chained_entry));
      new_entry->actor = argv[0].p;
      new_entry->closure = argv[1].p;
      set_add(chained, new_entry);
      break;
    }
  case FUT_MSG_BLOCK:
    {
      if (DEBUG_PRINT) fprintf(stderr, "[%p]\tGot block from %p\n", pthread_self(), argv[0].p);

      if (fulfilled((future*) this)) {
	if (DEBUG_PRINT) fprintf(stderr, "[%p]\tReaching blocking but future is fulfilled\n", pthread_self());

	blocked_entry new_entry = { .actor = argv[0].p , .context = argv[1].p };
	resume(&new_entry);
	
	break;
      }

      // TODO: Record the actor as one that needs to be woken up when the
      // future value is set
      if (DEBUG_PRINT) fprintf(stderr, "[%p]\tBlocking on a future\n", pthread_self());
      Set blocked = getBlocked(this);
      blocked_entry *new_entry = pony_alloc(sizeof(blocked_entry));
      new_entry->actor = argv[0].p;
      new_entry->context = (stacklet_t*) argv[1].p;
      set_add(blocked, new_entry);
      break;
    }
  case FUT_MSG_YIELD:
    {
      // TODO: Record the actor as one that should be sent the resume
      // message (2nd argument) when the future value is set
      // the yielded set can be merged with the blocked set
      if (DEBUG_PRINT) fprintf(stderr, "[%p]\tYielding on a future\n", pthread_self());
      Set yielded = getYielded(this);
      blocked_entry *new_entry = pony_alloc(sizeof(blocked_entry));
      new_entry->actor = argv[0].p;
      new_entry->context = (stacklet_t*) argv[1].p;
      set_add(yielded, new_entry);
      break;
    }
  case FUT_MSG_FULFIL:
    {

      void *value = ((future*)this)->payload.value;

      // TODO:
      // - put all blocking actors back into the scheduler queue
      // - send the appropriate resume messages to all yielding actors
      if (DEBUG_PRINT) fprintf(stderr, "[%p]\tFulfilling on a future\n", pthread_self());

      Set chained = getChained(this);
      set_forall(chained, (void *((*)(void *, void*))) run_chain, value);

      Set yielded = getYielded(this);
      set_forall(yielded, (void *((*)(void *, void*))) resume, NULL);

      Set blocked = getBlocked(this);
      set_forall(blocked, (void *((*)(void *, void*))) resume, NULL);

      break;
    }
  case FUT_MSG_RESUME:
    {
      if (DEBUG_PRINT) fprintf(stderr, "[%p]\tResuming because we got a resume message from a future\n", pthread_self());
      resume(argv[0].p);
      break;
    }
  case FUT_MSG_RUN_CLOSURE:
    {
      // void *closure = argv[0].p;
      // XXX do what's necessary to run the closure obj
      // XXX this should probably trigger an error in a future object! 
      if (DEBUG_PRINT) fprintf(stderr, "[%p]\tThis point should not be reached!\n", pthread_self());
      assert(false);
      break;
    }
  }
}
