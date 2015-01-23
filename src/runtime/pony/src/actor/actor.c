// future_chaining
#define _XOPEN_SOURCE 800
#include <ucontext.h>
#include <sysexits.h>

#include "actor.h"
#include "messageq.h"
#include "../sched/scheduler.h"
#include "../mem/pool.h"
#include "../gc/cycle.h"
#include "../gc/trace.h"
#include <string.h>
#include <stdio.h>
#include <assert.h>

// future_chaining
#define Stack_Size 64*1024

enum
{
  FLAG_BLOCKED = 1 << 0,
  FLAG_SYSTEM = 1 << 1,
  FLAG_UNSCHEDULED = 1 << 2,
  FLAG_PENDINGDESTROY = 1 << 3,
};

typedef struct stack_page {
  void *stack;
  struct stack_page *next;
} stack_page;

struct pony_actor_t
{
  void* p;
  heap_t heap;
  gc_t gc;
  struct pony_actor_t* next;
  uint8_t flags;

  // keep things accessed by other actors on a separate cache line
  messageq_t q __attribute__ ((aligned (64)));
  message_t* continuation;
  pony_actor_type_t* actor_type;

  // future_chaining
  //temporary addition here before merge to new PonyRT
  ucontext_t ctx;
  ucontext_t home_ctx;
  bool resume;
  bool run_to_completion;
  stack_page *page;
};

#define MAX_STACK_IN_POOL 4

static __thread stack_page *stack_pool = NULL;
static __thread unsigned int available_pages = 0;
static __thread stack_page *local_page = NULL;
static __thread pony_actor_t* this_actor;

void* pony_get_messageq(){
  pony_actor_t* actor = actor_current();
  return &actor->q;
}

static stack_page *pop_page()
{
  if (available_pages == 0) {
    stack_pool = POOL_ALLOC(sizeof *stack_pool);
    stack_pool->next = NULL;
    posix_memalign(&stack_pool->stack, 16, Stack_Size);
  } else {
    // update the remaining number of pages
    available_pages--;
  }
  stack_page *page = stack_pool;
  stack_pool = page->next;
  return page;
}

static void push_page(stack_page *page)
{
  available_pages++;
  page->next = stack_pool;
  stack_pool = page;
}

void reclaim_page(pony_actor_t *a)
{
  push_page(a->page);
  a->page = NULL;
}

static void clean_pool()
{
  static stack_page *page, *next;
  while (available_pages > MAX_STACK_IN_POOL) {
    available_pages--;
    page = stack_pool;
    next = stack_pool->next;
    free(page->stack);
    POOL_FREE(stack_page, page);
    stack_pool = next;
  }
}

void actor_suspend(pony_actor_t *actor)
{
  // Make a copy of the current context and replace it
  ucontext_t ctxp = actor->ctx;

  ctx_wrapper d = { .ctx = &ctxp, .uc_link=ctxp.uc_link };
  pony_arg_t argv[1] =  { { .p = &d } };
  actor->page = local_page;
  local_page = NULL;

  pony_sendv(actor, FUT_MSG_SUSPEND, 1, argv);

  actor->run_to_completion = false;
  assert(swapcontext(&ctxp, ctxp.uc_link) == 0);
  assert(ctxp.uc_link == &actor->home_ctx);
}

// TODO: suspend and await overlaps heavily
void actor_await(pony_actor_t *actor, void *future)
{
  // Make a copy of the current context and replace it
  ucontext_t ctxp = actor->ctx;
  ctx_wrapper d = { .ctx = &ctxp, .uc_link=ctxp.uc_link };
  pony_arg_t argv[2] =  { { .p = &d }, { .p = future } };

  actor->page = local_page;
  local_page = NULL;

  pony_sendv(actor, FUT_MSG_AWAIT, 2, argv);

  actor->run_to_completion = false;
  assert(swapcontext(&ctxp, ctxp.uc_link) == 0);
  assert(ctxp.uc_link == &actor->home_ctx);
}

void actor_block(pony_actor_t *actor)
{
  actor->page = local_page ? local_page : actor->page;
  assert(actor->page);
  local_page = NULL;
  actor->run_to_completion = false;
  assert(swapcontext(&actor->ctx, actor->ctx.uc_link) == 0);
}

void actor_set_resume(pony_actor_t *actor)
{
  actor->resume = true;
}

void actor_set_run_to_completion(pony_actor_t *actor)
{
    actor->run_to_completion = true;
}

bool actor_run_to_completion(pony_actor_t *actor)
{
    return actor->run_to_completion;
}

void actor_resume(pony_actor_t *actor)
{
  actor->resume = false;
  actor->run_to_completion = true;
  assert(swapcontext(actor->ctx.uc_link, &actor->ctx) == 0);

  if (actor->run_to_completion) {
    reclaim_page(actor);
  }
}

static bool has_flag(pony_actor_t* actor, uint8_t flag)
{
  return (actor->flags & flag) != 0;
}

static void set_flag(pony_actor_t* actor, uint8_t flag)
{
  actor->flags |= flag;
}

static void unset_flag(pony_actor_t* actor, uint8_t flag)
{
  actor->flags &= ~flag;
}

static void prep_message(pony_actor_t* to, uint64_t id,
  int argc, pony_arg_t* argv)
{
  if(id >= ACTORMSG_ACQUIRE)
    return;

  pony_msg_t* mtype = to->actor_type->msg(id);
  assert(argc <= PONY_MAX_ARG);
  assert(mtype != NULL);
  assert(mtype->argc == argc);

  if(argc == 0)
    return;

  gc_t* gc = &this_actor->gc;
  heap_t* heap = &this_actor->heap;
  trace_send();

  for(int i = 0; i < argc; i++)
  {
    if(mtype->type[i] == PONY_NONE)
    {
      // no gc_send needed
    } else if(mtype->type[i] == PONY_ACTOR) {
      gc_sendactor(this_actor, gc, argv[i].p);
    } else {
      gc_sendobject(this_actor, heap, gc, argv[i].p, mtype->type[i]->trace);
    }
  }

  // send the acquire map
  gc_sendacquire();

  // clear gc marks
  gc_done(gc);
}

static void push_message(pony_actor_t* to, uint64_t id,
  int argc, pony_arg_t* argv)
{
  if(messageq_push(&to->q, id, argc, argv))
  {
    if(!has_flag(to, FLAG_UNSCHEDULED))
      scheduler_add(to);
  }
}


static bool handle_message(pony_actor_t* actor, message_t* msg)
{
  switch(msg->id)
  {
    case ACTORMSG_ACQUIRE:
      // if we are blocked and our rc changes, block again
      if(gc_acquire(&actor->gc, msg->argv[0].p) &&
        has_flag(actor, FLAG_BLOCKED))
      {
        cycle_block(actor, &actor->gc);
      }
      return false;

    case ACTORMSG_RELEASE:
      // if we are blocked and our rc changes, block again
      if(gc_release(&actor->gc, msg->argv[0].p) &&
        has_flag(actor, FLAG_BLOCKED))
      {
        cycle_block(actor, &actor->gc);
      }
      return false;

    case ACTORMSG_CONF:
      cycle_ack(msg->argv[0].i);
      return false;

    case FUT_MSG_SUSPEND:
      future_suspend_resume(msg->argv[0].p);
      return true;

    case FUT_MSG_AWAIT:
      future_await_resume(msg->argv);
      return true;

    case FUT_MSG_RUN_CLOSURE:
      run_closure(msg->argv[0].p, msg->argv[1].p, msg->argv[2].p);
      return true;

    default:
    {
      local_page = local_page ? local_page : pop_page();

      if(has_flag(actor, FLAG_BLOCKED))
      {
        // if we are blocked and we get an application message, unblock
        cycle_unblock(actor);
        unset_flag(actor, FLAG_BLOCKED);
      }

      pony_msg_t* mtype = actor->actor_type->msg(msg->id);
      assert(mtype->argc <= PONY_MAX_ARG);

      if(mtype->argc > 0)
      {
        trace_recv();

        for(int i = 0; i < mtype->argc; i++)
        {
          if(mtype->type[i] == PONY_NONE)
          {
            // no gc_recv needed
          } else if(mtype->type[i] == PONY_ACTOR) {
            gc_recvactor(actor, &actor->gc, msg->argv[i].p);
          } else {
            gc_recvobject(actor, &actor->heap, &actor->gc,
              msg->argv[i].p, mtype->type[i]->trace);
          }
        }

        // clear gc marks
        gc_done(&actor->gc);
      }

      // future_chaining
      // Temporary insertion, should go into each actor
      getcontext(&actor->ctx);
      actor->ctx.uc_stack.ss_sp = local_page->stack;
      actor->ctx.uc_stack.ss_size = Stack_Size;
      actor->ctx.uc_link = &actor->home_ctx;
      actor->ctx.uc_stack.ss_flags = 0;
      makecontext(&actor->ctx, actor->actor_type->dispatch, 5, actor, actor->p, msg->id, mtype->argc, msg->argv);

      if (swapcontext(&actor->home_ctx, &actor->ctx) != 0) {
          err(EX_OSERR, "swapcontext failed");
      }
      return true;
    }
  }
}

bool actor_run(pony_actor_t* actor)
{
  message_t* msg;
  this_actor = actor;

  if(actor->resume)
  {
    actor_resume(actor);
    return !has_flag(actor, FLAG_UNSCHEDULED);
  }

  clean_pool();

  if(heap_startgc(&actor->heap))
  {
    trace_mark();
    gc_markobject(actor, &actor->heap, &actor->gc, actor->p,
      actor->actor_type->type.trace);
    gc_mark(&actor->gc);
    gc_sweep(&actor->gc);
    gc_done(&actor->gc);
    heap_endgc(&actor->heap);
  }

  while((msg = messageq_pop(&actor->q)) != NULL)
  {
    if(handle_message(actor, msg))
      return !has_flag(actor, FLAG_UNSCHEDULED);
  }

  if(!has_flag(actor, FLAG_BLOCKED | FLAG_SYSTEM | FLAG_UNSCHEDULED))
  {
    cycle_block(actor, &actor->gc);
    set_flag(actor, FLAG_BLOCKED);
  } else if(has_flag(actor, FLAG_UNSCHEDULED)) {
    // when unscheduling, don't mark the queue as empty, since we don't want
    // to get rescheduled if we receive a message
    return false;
  }

  return !messageq_markempty(&actor->q);
}

void actor_destroy(pony_actor_t* actor)
{
  assert(has_flag(actor, FLAG_PENDINGDESTROY));

  messageq_destroy(&actor->q);
  heap_destroy(&actor->heap);

  POOL_FREE(pony_actor_t, actor);
}

pony_actor_t* actor_current()
{
  return this_actor;
}

gc_t* actor_gc(pony_actor_t* actor)
{
  return &actor->gc;
}

heap_t* actor_heap(pony_actor_t* actor)
{
  return &actor->heap;
}

bool actor_pendingdestroy(pony_actor_t* actor)
{
  return has_flag(actor, FLAG_PENDINGDESTROY);
}

void actor_setpendingdestroy(pony_actor_t* actor)
{
  set_flag(actor, FLAG_PENDINGDESTROY);
}

bool actor_hasfinal(pony_actor_t* actor)
{
  return actor->actor_type->final != NULL;
}

void actor_final(pony_actor_t* actor)
{
  actor->actor_type->final(actor->p);
}

void actor_sweep(pony_actor_t* actor)
{
  gc_sweep(&actor->gc);
}

void actor_setsystem(pony_actor_t* actor)
{
  set_flag(actor, FLAG_SYSTEM);
}

pony_actor_t* actor_next(pony_actor_t* actor)
{
  return actor->next;
}

void actor_setnext(pony_actor_t* actor, pony_actor_t* next)
{
  actor->next = next;
}

void actor_inc_rc()
{
  this_actor->gc.rc++;
}

void actor_dec_rc()
{
  this_actor->gc.rc--;
}

pony_actor_t* pony_create(pony_actor_type_t* type)
{
  assert(type != NULL);

  pony_actor_t* actor = POOL_ALLOC(pony_actor_t);
  memset(actor, 0, sizeof(pony_actor_t));
  actor->actor_type = type;

  messageq_init(&actor->q);
  heap_init(&actor->heap);

  if(this_actor != NULL)
  {
    // actors begin unblocked and referenced by the creating actor
    actor->gc.rc = GC_INC_MORE;
    gc_createactor(&this_actor->gc, actor);
  } else {
    // no creator, so the actor isn't referenced by anything
    actor->gc.rc = 0;
  }

  // future_chaining
  actor->resume = false;

  return actor;
}

void* pony_get()
{
  return this_actor->p;
}

void pony_set(void* p)
{
  this_actor->p = p;
}

void pony_send(pony_actor_t* to, uint64_t id)
{
  push_message(to, id, 0, NULL);
}

void pony_sendv(pony_actor_t* to, uint64_t id, int argc, pony_arg_t* argv)
{
  prep_message(to, id, argc, argv);
  push_message(to, id, argc, argv);
}

void pony_sendp(pony_actor_t* to, uint64_t id, void* p)
{
  pony_arg_t arg = {.p = p};
  pony_sendv(to, id, 1, &arg);
}

void pony_sendi(pony_actor_t* to, uint64_t id, intptr_t i)
{
  pony_arg_t arg = {.i = i};
  push_message(to, id, 1, &arg);
}

void pony_sendd(pony_actor_t* to, uint64_t id, double d)
{
  pony_arg_t arg = {.d = d};
  push_message(to, id, 1, &arg);
}

void pony_continuation(pony_actor_t* to, uint64_t id,
  int argc, pony_arg_t* argv)
{
  prep_message(to, id, argc, argv);

  message_t* msg = POOL_ALLOC(message_t);
  msg->id = id;
  memcpy(msg->argv, argv, argc * sizeof(pony_arg_t));
  msg->next = NULL;

  assert(to->continuation == NULL);
  to->continuation = msg;
}

void* pony_alloc(size_t size)
{
  return heap_alloc(this_actor, &this_actor->heap, size);
}

void pony_triggergc()
{
  this_actor->heap.next_gc = 0;
}

void pony_schedule(pony_actor_t* actor)
{
  if(!has_flag(actor, FLAG_UNSCHEDULED))
    return;

  unset_flag(actor, FLAG_UNSCHEDULED);
  scheduler_add(actor);
}

void pony_unschedule()
{
  if(has_flag(this_actor, FLAG_BLOCKED))
  {
    cycle_unblock(this_actor);
    unset_flag(this_actor, FLAG_BLOCKED);
  }

  set_flag(this_actor, FLAG_UNSCHEDULED);
}

void pony_become(pony_actor_t* actor)
{
  this_actor = actor;
}

bool pony_poll()
{
  return actor_run(this_actor);
}
