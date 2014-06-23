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
#include "closure.h"
#include "pthread.h"

#define _XOPEN_SOURCE

// Debug
void print_threadid() {
  fprintf(stderr, "{{{ current thread: %p }}}\n", pthread_self());
}

void print_value(value args[], void* env) {
  fprintf(stderr, "------------------> Chained closure prints value as: %llu\n", args[0].integer);
}

typedef struct state_t
{
  pony_actor_t *value_producer;
  pony_actor_t *other_actor;
  bool done;
} state_t;

enum
  {
    MSG_ASYNC_CALL,
    MSG_FUTURE_ARG,
    MSG_SELF_CALL,
    MSG_START
  };

static void trace(void* p);
static pony_msg_t* message_type(uint64_t id);
static void trampoline(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv);

static pony_actor_type_t type =
  {
    1,
    {trace, sizeof(state_t), PONY_ACTOR},
    message_type,
    trampoline
  };

static pony_msg_t m_future_arg = {1, {{NULL, 0, PONY_ACTOR}} };
static pony_msg_t m_async_call = {1, {{NULL, 0, PONY_ACTOR}} };
static pony_msg_t m_self_call = {1, {{NULL, 0, PONY_PRIMITIVE}} };
static pony_msg_t m_resume_get = {1, {{NULL, 0, PONY_PRIMITIVE}} };
static pony_msg_t m_start = {0, {{NULL, 0, PONY_PRIMITIVE}} };
static pony_msg_t m_closure_run = {2, {{NULL, 0, PONY_ACTOR}, {NULL, 0, PONY_PRIMITIVE}} };

static void trace(void* p)
{
  fprintf(stderr, "Tracing in Main or Server\n");
  state_t* d = p;
  pony_traceactor(&d->value_producer);
  pony_traceactor(&d->other_actor);
}

static pony_msg_t* message_type(uint64_t id)
{
  switch(id)
    {
    case FUT_MSG_RUN_CLOSURE: return  &m_closure_run;
    case FUT_MSG_RESUME: return &m_resume_get;
    case MSG_ASYNC_CALL: return &m_async_call;
    case MSG_FUTURE_ARG: return &m_future_arg;
    case MSG_SELF_CALL:  return &m_self_call;
    case MSG_START:      return &m_start;
    }

  return NULL;
}

static state_t *actors_init(void *p, void *who) {
  if (p == NULL) {
    /* fprintf(stderr, "[%p]\tInitialising state for %p\n", pthread_self(), who); */
    state_t *state = pony_alloc(sizeof(state_t));
    state->value_producer = NULL;
    state->other_actor = NULL;
    pony_set(state);
    return state;
  } else {
    return (state_t*) p;
  }
}

static void futures_eager_dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv) {
  state_t *d = p;

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
	fprintf(stderr, "[%p]\tstart ---> %p \n", pthread_self(), this);
        pony_actor_t* value_producer = pony_create(&type);
	fprintf(stderr, "[%p]\tServer is: %p\n", pthread_self(), value_producer);
        d->value_producer = value_producer;
        // Create a future and asynchronously call value_producer
        future_t* fut = future_mk();
	fprintf(stderr, "[%p]\tFuture is: %p\n", pthread_self(), fut);

	struct closure *closure = closure_mk((closure_fun) print_value, NULL);
	fprintf(stderr, "[%p]\tClosure is: %p\n", pthread_self(), closure);
	future_chain(fut, this, closure);
	
        fprintf(stderr, "[%p]\tValue in fresh pony_actor_t: %d\n", pthread_self(), (int) future_read_value(fut));

        pony_arg_t args[1];
        args[0].p = fut;
	fprintf(stderr, "[%p]\t%p <--- async call (%p) from %p\n", pthread_self(), d->value_producer, fut, this);
        pony_sendv(d->value_producer, MSG_ASYNC_CALL, 1, args);

        // for (int i = 0; i<10; ++i) pony_sendv(this, MSG_SELF_CALL, 1, args);
	// getchar();

        fprintf(stderr, "[%p]\t.....\n", pthread_self());

        // block	
	if (!future_fulfilled(fut)) {
	  print_threadid();
	  future_block(fut, this, NULL);
	  print_threadid();
	}

        fprintf(stderr, "[%p]\tReturning from blocking\n", pthread_self());
        fprintf(stderr, "[%p]\tPopulated: %d\n", pthread_self(), future_fulfilled(fut));
        fprintf(stderr, "[%p]\tValue: %d\n", pthread_self(), (int) future_read_value(fut));

        break;
      }

    case FUT_MSG_RUN_CLOSURE:
      {
	fprintf(stderr, "[%p]\t(%p) run_closure ---> %p \n", pthread_self(), argv[0].p, this);
	struct closure* closure = argv[0].p;
	value closure_arguments[1];
	closure_arguments[0].ptr = argv[1].p;
        closure_call(closure, closure_arguments); 
        break;
      }

    case MSG_ASYNC_CALL:
      {
	fprintf(stderr, "[%p]\t(%p) async_call ---> %p \n", pthread_self(), argv[0].p, this);
        // perform long-running calculation, set pony_actor_t value
        future_t *fut = (future_t*) argv[0].p;
	future_fulfil(fut, (void*) 1024);
        break;
      }

    case MSG_FUTURE_ARG:
      {
	fprintf(stderr, "[%p]\t(%p) future_arg ---> %p \n", pthread_self(), argv[0].p, this);
        // also block on the pony_actor_t
        future_t *fut = (future_t*) argv[0].p;
        future_block(fut, this, NULL);
        break;
      }

    case FUT_MSG_RESUME:
      {
	fprintf(stderr, "[%p]\t(%p) resume ---> %p \n", pthread_self(), argv[0].p, this);
	resumable_t *r = argv[0].p;
	fprintf(stderr, "Resuming on %p\n", r);
	future_resume(r);
	fprintf(stderr, "[%p]\tDone resuming\n", pthread_self());
	break;
      }

    case MSG_SELF_CALL:
      {
	fprintf(stderr, "[%p]\tself_call ---> %p \n", pthread_self(), this);
        fprintf(stderr, "[%p]\tSelf calling!\n", pthread_self());
        break;
      }

    }
}

static __thread bool called_init;

static void trampoline(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv) {
  // lazy_t_done(current);

  if (!called_init) {
    // lazy_t_init_current();
    called_init = true;
  }

  fprintf(stderr, "(%p)\t---------------------- TRAMPOLINE in (%p) ----------------------\n", pthread_self(), this);

  state_t *state = actors_init(p, this);
  /* futures_eager_dispatch(this, state, id, argc, argv); */
  /* return; */
  
  /* if (state->stacklet && lazy_t_is_suspended(state->stacklet)) { */
  /*   fprintf(stderr, "<<%p>>\tDispatch abandons current thread for existing stacklet %p\n", pthread_self(), state->stacklet); */
  /*   lazy_tit_t *stacklet = state->stacklet; */
  /*   state->stacklet = NULL; */
  /*   lazy_t_resume(stacklet); */
  /* } else { */
    futures_eager_dispatch(this, state, id, argc, argv);
  /* } */
}

int main(int argc, char** argv)
{
  init_futures(2, LAZY);

  pony_actor_t* main = pony_create(&type);
  fprintf(stderr, "[%p]\tMain is: %p\n", pthread_self(), main);
  int result = pony_start(argc, argv, main);
  return result;
}
