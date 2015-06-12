#define _XOPEN_SOURCE 800
#include <pony.h>
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

#ifdef LAZY_IMPL
__attribute__ ((noreturn))
static void actor_resume(encore_actor_t *actor);
__attribute__ ((noreturn))
static void actor_suspend_resume(encore_actor_t *actor, ucontext_t *ctx);
__attribute__ ((noreturn))
static void actor_await_resume(encore_actor_t *actor, ucontext_t *ctx);
#else
static void actor_resume(encore_actor_t *actor);
static void actor_suspend_resume(encore_actor_t *actor, ucontext_t *ctx);
static void actor_await_resume(encore_actor_t *actor, ucontext_t *ctx);
#endif

#ifdef LAZY_IMPL
__attribute__ ((noreturn))
static void actor_resume_context(encore_actor_t *actor, ucontext_t *ctx);
#else
static void actor_resume_context(encore_actor_t *actor, ucontext_t *ctx);
#endif

extern void public_run(pony_actor_t *actor);

extern bool pony_system_actor(pony_actor_t *actor);

#define MAX_IN_POOL 4

inline static void assert_swap(ucontext_t *old, ucontext_t *new)
{
  int ret = swapcontext(old, new);
  assert(ret == 0);
}

static __pony_thread_local context *context_pool = NULL;
static __pony_thread_local unsigned int available_context = 0;

__pony_thread_local context *root_context;
__pony_thread_local context *this_context;

void actor_unlock(encore_actor_t *actor)
{
  if (!pony_system_actor((pony_actor_t*) actor)) {
    if (actor->lock) {
      pthread_mutex_t *lock = actor->lock;
      actor->lock = NULL;
      pthread_mutex_unlock(lock);
    }
  }
}

void pony_sendargs(pony_actor_t* to, uint32_t id, int argc, char** argv)
{
  pony_main_msg_t* m = (pony_main_msg_t*)pony_alloc_msg(
          POOL_INDEX(sizeof(pony_main_msg_t)), id);
  m->argc = argc;
  m->argv = argv;

  pony_sendv(to, &m->msg);
}

encore_arg_t default_task_handler(void* env, void* dep){
  return run_closure(((struct default_task_env_s*)env)->fn, ((struct default_task_env_s*)env)->value); // don't know the type returned by the closure
}

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

#endif

#ifdef LAZY_IMPL

static context *pop_context(encore_actor_t *actor)
{
  context *c;
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
  makecontext(&context_pool->ctx, (void(*)(void))public_run, 1, actor);
  c = context_pool;
  context_pool = c->next;
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

  stack_page *page, *next;
  while (available_pages > MAX_IN_POOL) {
    available_pages--;
    page = stack_pool;
    next = stack_pool->next;
    free(page->stack);
    free(page);
    stack_pool = next;
  }

#else

  context *ctx, *next;
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

#if defined(PLATFORM_IS_MACOSX)
__attribute__ ((noinline))
static void force_thread_local_variable_access(context *old_this_context,
    context *old_root_context)
{
  this_context = old_this_context;
  root_context = old_root_context;
  this_context->ctx.uc_stack.ss_sp = old_this_context->ss_sp;
}
#endif

void actor_save_context(encore_actor_t *actor, ucontext_t *ctx)
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
  assert_swap(ctx, &actor->home_ctx);

#else

#if defined(PLATFORM_IS_MACOSX)
  this_context->ss_sp = this_context->ctx.uc_stack.ss_sp;
#endif

  context *old_this_context = this_context;
  context *old_root_context = root_context;
  encore_actor_t *old_actor = actor;
  this_context = pop_context(actor);
  assert_swap(ctx, &this_context->ctx);
#if defined(PLATFORM_IS_MACOSX)
  force_thread_local_variable_access(old_this_context, old_root_context);
#else
  this_context = old_this_context;
  root_context = old_root_context;
#endif
  pony_become((pony_actor_t *) old_actor);

#endif
}

void actor_block(encore_actor_t *actor)
{

#ifndef LAZY_IMPL
  actor_save_context(actor, &actor->ctx);
#else
  actor->saved = &this_context->ctx;
  actor_save_context(actor, actor->saved);
#endif

}

void actor_suspend()
{
  encore_actor_t *actor = (encore_actor_t *) actor_current();
  actor->suspend_counter++;

  ucontext_t ctx;
  pony_sendp((pony_actor_t*) actor, _ENC__MSG_RESUME_SUSPEND, &ctx);

  actor_save_context(actor, &ctx);

  actor->suspend_counter--;
  assert(actor->suspend_counter >= 0);
}

void actor_await(ucontext_t *ctx)
{
  encore_actor_t *actor = (encore_actor_t *) actor_current();
  actor->await_counter++;

  actor_save_context(actor, ctx);

  actor->await_counter--;

  assert(actor->await_counter >= 0);
}

void actor_set_resume(encore_actor_t *actor)
{
  actor->resume = true;
}

static void actor_resume_context(encore_actor_t* actor, ucontext_t *ctx)
{
#ifndef LAZY_IMPL

  actor->run_to_completion = true;

  assert_swap(&actor->home_ctx, ctx);

  if (actor_run_to_completion(actor)) {
    reclaim_page(actor);
  }

#else

  if (this_context != root_context) {
    push_context(this_context);
  }
  setcontext(ctx);
  assert(0);
  exit(-1);

#endif
}

static void actor_resume(encore_actor_t *actor)
{
  actor->resume = false;
#ifndef LAZY_IMPL
  assert(actor->ctx.uc_link == &actor->home_ctx);
  actor_resume_context(actor, &actor->ctx);
#else
  actor_resume_context(actor, actor->saved);
#endif
}

static void actor_suspend_resume(encore_actor_t *actor, ucontext_t *ctx)
{
  actor_resume_context(actor, ctx);
}

static void actor_await_resume(encore_actor_t *actor, ucontext_t *ctx)
{
  actor_resume_context(actor, ctx);
}

bool gc_disabled()
{
  encore_actor_t *actor = (encore_actor_t*) actor_current();
  return actor->suspend_counter > 0 || actor->await_counter > 0;
}

encore_actor_t *encore_create(pony_type_t *type)
{
  encore_actor_t *new = (encore_actor_t *)pony_create(type);
  new->_enc__self_type = type;
  return new;
}

encore_actor_t *encore_peer_create(pony_type_t *type)
{
  //todo: this should create an actor in another work pool
  // printf("warning: creating peer not implemented by runtime\n");
  encore_actor_t *new = (encore_actor_t *)pony_create(type);
  new->_enc__self_type = type;
  return new;
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

  return pony_start(false);
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
      actor_suspend_resume(actor, ((pony_msgp_t*)msg)->p);
      return true;

    case _ENC__MSG_RESUME_AWAIT:
      actor_await_resume(actor, ((pony_msgp_t*)msg)->p);
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
