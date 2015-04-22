#include <pony/pony.h>
#include "encore.h"
#include "closure.h"
#include "actor/actor.h"
#include "sched/scheduler.h"
#include "mem/pool.h"
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include <ucontext.h>
#include <stdio.h>
#include "task.h"

extern void public_run(pthread_mutex_t *lock);

enum
{
  FLAG_BLOCKED = 1 << 0,
  FLAG_SYSTEM = 1 << 1,
  FLAG_UNSCHEDULED = 1 << 2,
  FLAG_PENDINGDESTROY = 1 << 3,
};

#define MAX_IN_POOL 4

inline static void assert_swap(ucontext_t *old, ucontext_t *new)
{
  int ret = swapcontext(old, new);
  assert(ret == 0);
}

static __pony_thread_local context *context_pool = NULL;
static __pony_thread_local unsigned int available_context = 0;

__pony_thread_local context *this_context;
__pony_thread_local ucontext_t *origin;
__pony_thread_local ucontext_t *root;

#ifndef LAZY_IMPL

static __pony_thread_local stack_page *stack_pool = NULL;
static __pony_thread_local unsigned int available_pages = 0;
static __pony_thread_local stack_page *local_page = NULL;

static stack_page *pop_page()
{
  if (available_pages == 0) {
    stack_pool = malloc(sizeof *stack_pool);
    stack_pool->next = NULL;
    int ret = posix_memalign(&stack_pool->stack, 16, Stack_Size);
    assert(ret == 0);
  } else {
    available_pages--;
  }
  stack_page *page = stack_pool;
  stack_pool = page->next;
  assert(page->stack);
  return page;
}

static void push_page(stack_page *page)
{
  available_pages++;
  page->next = stack_pool;
  stack_pool = page;
}

static void reclaim_page(encore_actor_t *a)
{
  push_page(a->page);
  a->page = NULL;
}

void *get_local_page_stack()
{
    local_page = local_page ? local_page : pop_page();
    assert(local_page);
    assert(local_page->stack);
    return local_page->stack;
}

void actor_set_run_to_completion(encore_actor_t *actor)
{
  actor->run_to_completion = true;
}

bool actor_run_to_completion(encore_actor_t *actor)
{
  return actor->run_to_completion;
}

void actor_unlock(encore_actor_t *actor)
{
  if (actor->lock) {
    pthread_mutex_unlock(actor->lock);
    actor->lock = NULL;
  }
}

#endif

#ifdef LAZY_IMPL

static context *pop_context(encore_actor_t *actor)
{
  static context *c;
  if (available_context == 0) {
    context_pool = malloc(sizeof *context_pool);
    context_pool->next = NULL;
    getcontext(&context_pool->ctx);
    int ret = posix_memalign((void *)&context_pool->ctx.uc_stack.ss_sp, 16, Stack_Size);
    assert(ret == 0);
    context_pool->ctx.uc_stack.ss_size = Stack_Size;
    context_pool->ctx.uc_stack.ss_flags = 0;
  } else {
    available_context--;
  }
  makecontext(&context_pool->ctx, (void(*)(void))public_run, 1, actor->lock);
  c = context_pool;
  context_pool = c->next;
  assert(c->ctx.uc_stack.ss_sp);
  return c;
}

static void push_context(context *ctx)
{
  available_context++;
  ctx->next = context_pool;
  context_pool = ctx;
}

#endif

static void clean_pool()
{
#ifndef LAZY_IMPL

  static stack_page *page, *next;
  while (available_pages > MAX_IN_POOL) {
    available_pages--;
    page = stack_pool;
    next = stack_pool->next;
    free(page->stack);
    free(page);
    stack_pool = next;
  }

#else

  static context *ctx, *next;
  while (available_context > MAX_IN_POOL) {
    available_context--;
    ctx = context_pool;
    next = context_pool->next;
    free(ctx->ctx.uc_stack.ss_sp);
    free(ctx);
    context_pool = next;
  }

#endif
}

void actor_block(encore_actor_t *actor)
{
#ifndef LAZY_IMPL

  if (!actor->page) {
    assert(local_page->stack);
    actor->page = local_page;
    local_page = NULL;
  }
  assert(actor->page);
  assert(actor->page->stack);
  actor->run_to_completion = false;
  assert_swap(&actor->ctx, actor->ctx.uc_link);

#else

  actor->saved = &this_context->ctx;

  context *old = this_context;

  this_context = pop_context(actor);
  assert_swap(actor->saved, &this_context->ctx);

  this_context = old;

#endif
}

void actor_set_resume(encore_actor_t *actor)
{
  actor->resume = true;
}

void actor_resume(encore_actor_t *actor)
{
#ifndef LAZY_IMPL

  actor->resume = false;
  actor->run_to_completion = true;
  assert_swap(actor->ctx.uc_link, &actor->ctx);

  if (actor->run_to_completion) {
    reclaim_page(actor);
  }

#else

  actor->resume = false;
  if (&this_context->ctx != root) {
    push_context(this_context);
  }
  setcontext(actor->saved);
  assert(0);

#endif
}

void actor_suspend()
{
  encore_actor_t *actor = (encore_actor_t *) actor_current();
  actor->suspend_counter++;

  ucontext_t ctx;

#ifndef LAZY_IMPL

  if (!actor->page) {
    assert(local_page->stack);
    actor->page = local_page;
    local_page = NULL;
  }

  assert(actor->page);
  assert(actor->page->stack);

  actor->run_to_completion = false;

  pony_sendp((pony_actor_t*) actor, _ENC__MSG_RESUME_SUSPEND, &ctx);

  assert_swap(&ctx, &actor->home_ctx);

  assert(actor->suspend_counter >= 0);

#else
  context *old = this_context;

  pony_sendp((pony_actor_t*) actor, _ENC__MSG_RESUME_SUSPEND, &ctx);

  this_context = pop_context(actor);
  scheduler_add((pony_actor_t*) actor);
  assert_swap(&ctx, &this_context->ctx);

  this_context = old;

#endif

  actor->suspend_counter--;
}

void actor_suspend_resume(ucontext_t *ctx)
{
  encore_actor_t *actor = (encore_actor_t *) actor_current();

#ifndef LAZY_IMPL

  actor_set_run_to_completion(actor);

  assert_swap(&actor->home_ctx, ctx);

  if (actor_run_to_completion(actor)) {
    reclaim_page(actor);
  }

#else

  if (&this_context->ctx != root) {
    push_context(this_context);
  }
  setcontext(ctx);
  assert(0);

#endif

}

// TODO: suspend and await overlaps heavily
void actor_await(encore_actor_t *actor, void *future)
{
#ifndef LAZY_IMPL
  // Make a copy of the current context and replace it
  ucontext_t ctxp = actor->ctx;
  ctx_wrapper d = { .ctx = &ctxp, .uc_link=ctxp.uc_link };
  encore_arg_t argv[2] =  { { .p = &d }, { .p = future } };

  actor->page = local_page;
  local_page = NULL;

  // TODO  find out the right way of calling pond_sendv
  // pony_sendv(actor, FUT_MSG_AWAIT, 2, argv);

  actor->run_to_completion = false;
  int ret = swapcontext(&ctxp, ctxp.uc_link);
  assert(ret == 0);
  assert(ctxp.uc_link == &actor->home_ctx);
#endif
}

bool gc_disabled()
{
  encore_actor_t *actor = (encore_actor_t*) actor_current();
  return actor->suspend_counter > 0;
}

encore_actor_t *encore_create(pony_type_t *type)
{
  return (encore_actor_t *)pony_create(type);
}

encore_actor_t *encore_peer_create(pony_type_t *type)
{
  //todo: this should create an actor in another work pool
  // printf("warning: creating peer not implemented by runtime\n");
  return (encore_actor_t *)pony_create(type);
}

/// Allocate s bytes of memory, zeroed out
void *encore_alloc(size_t s)
{
  void *mem = pony_alloc(s);
  memset(mem, 0, s);

  return mem;
}


/// The starting point of all Encore programs
int encore_start(int argc, char** argv, pony_type_t *type)
{
  argc = pony_init(argc, argv);
  task_setup(type);
  pony_actor_t* actor = (pony_actor_t *)encore_create(type);
  pony_sendargs(actor, _ENC__MSG_MAIN, argc, argv);

  return pony_start(PONY_DONT_WAIT);
}

bool encore_actor_run_hook(encore_actor_t *actor)
{
  clean_pool();

  if(actor->resume)
  {
    actor_resume(actor);
    return true;
  }

  return false;
}

bool encore_actor_handle_message_hook(encore_actor_t *actor, pony_msg_t* msg)
{
  switch(msg->id)
  {
    case _ENC__MSG_RESUME_SUSPEND:
      actor_suspend_resume(((pony_msgp_t*)msg)->p);
      return true;

    case _ENC__MSG_RESUME_AWAIT:
      assert(-1);
      // future_await_resume(msg->argv);
      return true;

    case _ENC__MSG_RUN_CLOSURE:
      assert(-1);
      // run_closure(msg->argv[0].p, msg->argv[1].p, msg->argv[2].p);
      return true;
  }
  return false;
}

void call_respond_with_current_scheduler()
{
  // TODO respond
  // respond(this_scheduler);
}
