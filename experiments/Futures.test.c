#define __STDC_FORMAT_MACROS
#include <pony/pony.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <inttypes.h>
#include <assert.h>
#include "future.h"
#include "ccontext.h"


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
  // leak for now
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

static void dispatch2(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv)
{
  fprintf(stderr, "********************************* DISPATCH in %p with %d *********************************\n", this, id);

  state_t* d = p;

  switch(id)
    {
     case PONY_MAIN:
      {
	fprintf(stderr, "%p <--- start \n", this);
	pony_send(this, MSG_START);
	break;
      }
     case MSG_START:
      {
        pony_actor_t* value_producer = pony_create(&type);
	fprintf(stderr, "Server is: %p\n", value_producer);
        d->value_producer = value_producer;
        // Create a future and asynchronously call value_producer
        future * fut = createNewFuture();
	fprintf(stderr, "Future is: %p\n", fut);
        fprintf(stderr, "Value in fresh future: %d\n", (int) getValue(fut));

        pony_arg_t args[1];
        args[0].p = fut;
	fprintf(stderr, "%p <--- async call (%p) from %p\n", d->value_producer, fut, this);
        pony_sendv(d->value_producer, MSG_ASYNC_CALL, 1, args);

        // for (int i = 0; i<10; ++i) pony_sendv(this, MSG_SELF_CALL, 1, args);
	// getchar();

        fprintf(stderr, ".....\n");

        // block	
	if (!populated(fut)) {
	  print_threadid();
	  block(fut, this);
	  print_threadid();
	}

        fprintf(stderr, "Returning from blocking\n");
        fprintf(stderr, "Populated: %d\n", populated(fut));
        fprintf(stderr, "Value: %d\n", (int) getValue(fut));

        break;
      }

    case MSG_ASYNC_CALL:
      {
        fprintf(stderr, "Returning a value by installing it in the future\n");
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
	fprintf(stderr, "Main is resuming on a future!\n");
	stacklet_t *context = argv[0].p;
	context->resumed = true;
	context->context.uc_link = &d->markPoint.context;
	setcontext(&context->context);
  //t_resume(context);
	fprintf(stderr, "Done resuming\n");
	break;
      }

    case MSG_SELF_CALL:
      {
        fprintf(stderr, "Self calling!\n");
        break;
      }

    }
}

static void dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv) {
  state_t *d = p;
  if (d == NULL) {
    p = d = pony_alloc(sizeof(state_t));
    pony_set(d);
  }

  t_checkpoint(&d->markPoint, dispatch2, this, d, id, argc, argv);
}

int main(int argc, char** argv)
{
  pony_actor_t* main = pony_create(&type);
  fprintf(stderr, "Main is: %p\n", main);
  return pony_start(argc, argv, main);
}
