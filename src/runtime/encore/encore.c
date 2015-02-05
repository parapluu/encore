#include <pony/pony.h>
#include "encore.h"
#include <string.h>
#include <stdbool.h>
#include <assert.h>

enum
{
  FLAG_BLOCKED = 1 << 0,
  FLAG_SYSTEM = 1 << 1,
  FLAG_UNSCHEDULED = 1 << 2,
  FLAG_PENDINGDESTROY = 1 << 3,
};

#define MAX_STACK_IN_POOL 4
#define Stack_Size 64*1024

static __pony_thread_local stack_page *stack_pool = NULL;
static __pony_thread_local unsigned int available_pages = 0;
static __pony_thread_local stack_page *local_page = NULL;

static stack_page *pop_page()
{
  if (available_pages == 0) {
    stack_pool = encore_alloc(sizeof *stack_pool);
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

void reclaim_page(encore_actor_t *a)
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
    pool_free(sizeof(stack_page), page);
    stack_pool = next;
  }
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
  assert(swapcontext(&ctxp, ctxp.uc_link) == 0);
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
  assert(swapcontext(&ctxp, ctxp.uc_link) == 0);
  assert(ctxp.uc_link == &actor->home_ctx);
}

void actor_block(encore_actor_t *actor)
{
  if (!actor->page) {
    actor->page = local_page;
    local_page = NULL;
  }
  assert(actor->page);
  actor->run_to_completion = false;
  assert(swapcontext(&actor->ctx, actor->ctx.uc_link) == 0);
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
  actor->resume = false;
  actor->run_to_completion = true;
  assert(swapcontext(actor->ctx.uc_link, &actor->ctx) == 0);

  if (actor->run_to_completion) {
    reclaim_page(actor);
  }
}

encore_actor_t *encore_create(pony_type_t *type)
{
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
  if(actor->resume)
  {
    // actor_resume(actor);
    return !has_flag(actor, FLAG_UNSCHEDULED);
  }

  // clean_pool();
  return true;
}
