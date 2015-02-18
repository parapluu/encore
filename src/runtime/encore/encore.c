#include <pony/pony.h>
#include "encore.h"
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include <ucontext.h>

extern void public_run();

bool has_flag(pony_actor_t* actor, uint8_t flag);

typedef struct context {
  ucontext_t ctx;
  struct context *next;
} context;

enum
{
  FLAG_BLOCKED = 1 << 0,
  FLAG_SYSTEM = 1 << 1,
  FLAG_UNSCHEDULED = 1 << 2,
  FLAG_PENDINGDESTROY = 1 << 3,
};

#define MAX_IN_POOL 4

static __pony_thread_local stack_page *stack_pool = NULL;
static __pony_thread_local unsigned int available_pages = 0;
static __pony_thread_local stack_page *local_page = NULL;

static __pony_thread_local context *context_pool = NULL;
static __pony_thread_local unsigned int available_context = 0;
static __pony_thread_local context *this_context = NULL;

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

void *get_local_page_stack()
{
    local_page = local_page ? local_page : pop_page();
    assert(local_page);
    assert(local_page->stack);
    return local_page->stack;
}

static void push_page(stack_page *page)
{
  available_pages++;
  page->next = stack_pool;
  stack_pool = page;
}

void reclaim_page(encore_actor_t *a)
{
  push_page(a->page);
  a->page = NULL;
}

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

void actor_suspend(encore_actor_t *actor)
{
  // Make a copy of the current context and replace it
  ucontext_t ctxp = actor->ctx;

  ctx_wrapper d = { .ctx = &ctxp, .uc_link=ctxp.uc_link };
  encore_arg_t argv[1] =  { { .p = &d } };
  actor->page = local_page;
  local_page = NULL;

  // TODO  find out the right way of calling pond_sendv
  // pony_sendv(actor, FUT_MSG_SUSPEND, 1, argv);

  actor->run_to_completion = false;
  int ret = swapcontext(&ctxp, ctxp.uc_link);
  assert(ret == 0);
  assert(ctxp.uc_link == &actor->home_ctx);
}

// TODO: suspend and await overlaps heavily
void actor_await(encore_actor_t *actor, void *future)
{
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
}

static context *pop_context()
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
    makecontext(&context_pool->ctx, (void(*)(void))public_run, 0);
  } else {
    available_context--;
  }
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
  int ret = swapcontext(&actor->ctx, actor->ctx.uc_link);
  assert(ret == 0);

#else

  ucontext_t ctx;
  actor->saved = &ctx;
  context *previous = this_context;
  this_context = pop_context();
  int ret = swapcontext(&ctx, &this_context->ctx);
  assert(ret == 0);
  this_context = previous;

#endif
}

void actor_set_resume(encore_actor_t *actor)
{
  actor->resume = true;
}

void actor_set_run_to_completion(encore_actor_t *actor)
{
    actor->run_to_completion = true;
}

bool actor_run_to_completion(encore_actor_t *actor)
{
    return actor->run_to_completion;
}

void actor_resume(encore_actor_t *actor)
{
#ifndef LAZY_IMPL

  actor->resume = false;
  actor->run_to_completion = true;
  int ret = swapcontext(actor->ctx.uc_link, &actor->ctx);
  assert(ret == 0);

  if (actor->run_to_completion) {
    reclaim_page(actor);
  }

#else

  actor->resume = false;
  if (this_context) {
    push_context(this_context);
  }
  setcontext(actor->saved);
  exit(1);

#endif
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
#ifndef LAZY_IMPL
    return !has_flag((pony_actor_t *)actor, FLAG_UNSCHEDULED);
#endif
  }

  return true;
}

bool encore_actor_handle_message_hook(encore_actor_t *actor, pony_msg_t* msg)
{
  switch(msg->id)
  {
    case _ENC__MSG_RESUME_SUSPEND:
      assert(-1);
      // future_suspend_resume(msg->argv[0].p);
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
