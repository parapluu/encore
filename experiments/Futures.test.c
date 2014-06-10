#define __STDC_FORMAT_MACROS
#include <pony/pony.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <inttypes.h>
#include <assert.h>
#include <pthread.h>
#include "future.h"
#include "ccontext.h"

// Debug
void print_threadid() {
  fprintf(stderr, "{{{ current thread: %p }}}\n", pthread_self());
}

typedef struct state_t
{
  stacklet_t markPoint;
  ucontext_t ctx;
  pony_actor_t *value_producer;
  pony_actor_t *other_actor;
  bool done;
} state_t;

enum
  {
    FUT_MSG_RUN_CLOSURE,
    FUT_MSG_RESUME,
    MSG_ASYNC_CALL,
    MSG_FUTURE_ARG,
    MSG_SELF_CALL,
    MSG_START
  };

static void trace(void* p);
static pony_msg_t* message_type(uint64_t id);
static void dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv);

static pony_actor_type_t type =
  {
    1,
    {trace, sizeof(state_t), PONY_ACTOR},
    message_type,
    dispatch
  };

static pony_msg_t m_future_arg = {1, {{NULL, 0, PONY_ACTOR}} };
static pony_msg_t m_async_call = {1, {{NULL, 0, PONY_ACTOR}} };
static pony_msg_t m_self_call = {1, {{NULL, 0, PONY_ACTOR}} };
static pony_msg_t m_resume_get = {1, {{NULL, 0, PONY_ACTOR}} };
static pony_msg_t m_start = {0, {{NULL, 0, PONY_PRIMITIVE}} };


static void trace(void* p)
{
  state_t* d = p;
  if (d->value_producer) pony_traceactor(d->value_producer);
  if (d->other_actor) pony_traceactor(d->other_actor);
}

static pony_msg_t* message_type(uint64_t id)
{
  switch(id)
    {
    case FUT_MSG_RUN_CLOSURE: return NULL; /// XXXX
    case FUT_MSG_RESUME: return &m_resume_get;
    case MSG_ASYNC_CALL: return &m_async_call;
    case MSG_FUTURE_ARG: return &m_future_arg;
    case MSG_SELF_CALL:  return &m_self_call;
    case MSG_START:      return &m_start;
    }

  return NULL;
}

static state_t *actors_init(void *p) {
  if (p == NULL) {
    state_t *state = pony_alloc(sizeof(state_t));
    state->value_producer = NULL;
    state->other_actor = NULL;
    pony_set(state);
    return state;
  } else {
    return (state_t*) p;
  }
}

static void dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv) {
  fprintf(stderr, "[%p]\t********************************* DISPATCH in %p with %d *********************************\n", pthread_self(), this, id);

  state_t *d = actors_init(p);

  // t_checkpoint(&d->markPoint, dispatch2, this, d, id, argc, argv);

  switch(id)
    {
     case PONY_MAIN:
      {
	fprintf(stderr, "[%p]\t%p <--- start \n", pthread_self(), this);
	pony_send(this, MSG_START);
	break;
      }
     case MSG_START:
      {
        pony_actor_t* value_producer = pony_create(&type);
	fprintf(stderr, "[%p]\tServer is: %p\n", pthread_self(), value_producer);
        d->value_producer = value_producer;
        // Create a future and asynchronously call value_producer
        future * fut = createNewFuture();
	fprintf(stderr, "[%p]\tFuture is: %p\n", pthread_self(), fut);
        fprintf(stderr, "[%p]\tValue in fresh future: %d\n", pthread_self(), (int) getValue(fut));

        pony_arg_t args[1];
        args[0].p = fut;
	fprintf(stderr, "[%p]\t%p <--- async call (%p) from %p\n", pthread_self(), d->value_producer, fut, this);
        pony_sendv(d->value_producer, MSG_ASYNC_CALL, 1, args);

        // for (int i = 0; i<10; ++i) pony_sendv(this, MSG_SELF_CALL, 1, args);
	// getchar();

        fprintf(stderr, "[%p]\t.....\n", pthread_self());

        // block	
	if (!fulfilled(fut)) {
	  print_threadid();
	  block(fut, this);
	  print_threadid();
	}

        fprintf(stderr, "[%p]\tReturning from blocking\n", pthread_self());
        fprintf(stderr, "[%p]\tPopulated: %d\n", pthread_self(), fulfilled(fut));
        fprintf(stderr, "[%p]\tValue: %d\n", pthread_self(), (int) getValue(fut));

        break;
      }

    case MSG_ASYNC_CALL:
      {
        fprintf(stderr, "[%p]\tReturning a value by installing it in the future\n", pthread_self());
        // perform long-running calculation, set future value
        future *fut = (future*) argv[0].p;
	fulfil(fut, (void*) 42);
        break;
      }

    case MSG_FUTURE_ARG:
      {
        // also block on the future
        future *fut = (future*) argv[0].p;
        block(fut, this);
        break;
      }

    case FUT_MSG_RESUME:
      {
	fprintf(stderr, "[%p]\tMain is resuming on a future!\n", pthread_self());
	stacklet_t *s = argv[0].p;
	t_resume(s);
	fprintf(stderr, "[%p] Done resuming\n", pthread_self());
	break;
      }

    case MSG_SELF_CALL:
      {
        fprintf(stderr, "[%p]\tSelf calling!\n", pthread_self());
        break;
      }

    }
}

int main(int argc, char** argv)
{
  pony_actor_t* main = pony_create(&type);
  fprintf(stderr, "[%p]\tMain is: %p\n", pthread_self(), main);
  return pony_start(argc, argv, main);
}
