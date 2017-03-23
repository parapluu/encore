#define PONY_WANT_ATOMIC_DEFS

#include "actor.h"
#include "../sched/scheduler.h"
#include "../sched/cpu.h"
#include "../mem/pool.h"
#include "../gc/cycle.h"
#include "../gc/trace.h"
#include "ponyassert.h"
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <dtrace.h>
#include "encore.h"

#ifdef USE_VALGRIND
#include <valgrind/helgrind.h>
#endif

enum
{
  FLAG_BLOCKED = 1 << 0,
  FLAG_RC_CHANGED = 1 << 1,
  FLAG_SYSTEM = 1 << 2,
  FLAG_UNSCHEDULED = 1 << 3,
  FLAG_PENDINGDESTROY = 1 << 4,
};

extern bool gc_disabled(pony_ctx_t *ctx);

static bool actor_noblock = false;

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
  actor->flags &= (uint8_t)~flag;
}

static bool handle_message(pony_ctx_t** ctx, pony_actor_t* actor,
  pony_msg_t* msg)
{
  if (!has_flag(actor, FLAG_SYSTEM)) {
    if (encore_actor_handle_message_hook((encore_actor_t*)actor, msg)) {
      return true;
    }
  }

  switch(msg->id)
  {
    case ACTORMSG_ACQUIRE:
    {
      pony_msgp_t* m = (pony_msgp_t*)msg;

      if(ponyint_gc_acquire(&actor->gc, (actorref_t*)m->p) &&
        has_flag(actor, FLAG_BLOCKED))
      {
        // If our rc changes, we have to tell the cycle detector before sending
        // any CONF messages.
        set_flag(actor, FLAG_RC_CHANGED);
      }

      return false;
    }

    case ACTORMSG_RELEASE:
    {
      pony_msgp_t* m = (pony_msgp_t*)msg;

      if(ponyint_gc_release(&actor->gc, (actorref_t*)m->p) &&
        has_flag(actor, FLAG_BLOCKED))
      {
        // If our rc changes, we have to tell the cycle detector before sending
        // any CONF messages.
        set_flag(actor, FLAG_RC_CHANGED);
      }

      return false;
    }

    case ACTORMSG_CONF:
    {
      if(has_flag(actor, FLAG_BLOCKED) && !has_flag(actor, FLAG_RC_CHANGED))
      {
        // We're blocked and our RC hasn't changed since our last block
        // message, send confirm.
        pony_msgi_t* m = (pony_msgi_t*)msg;
        ponyint_cycle_ack(*ctx, m->i);
      }

      return false;
    }

    default:
    {
      if(has_flag(actor, FLAG_BLOCKED))
      {
        // Send unblock before continuing. We no longer need to send any
        // pending rc change to the cycle detector.
        unset_flag(actor, FLAG_BLOCKED | FLAG_RC_CHANGED);
        ponyint_cycle_unblock(*ctx, actor);
      }

      DTRACE3(ACTOR_MSG_RUN, (uintptr_t)(*ctx)->scheduler, (uintptr_t)actor, msg->id);
      if (!has_flag(actor, FLAG_SYSTEM)) {
#ifndef LAZY_IMPL
        encore_actor_t *a = (encore_actor_t *)actor;
        getcontext(&a->uctx);
        a->uctx.uc_stack.ss_sp = get_local_page_stack();
        a->uctx.uc_stack.ss_size = Stack_Size;
        a->uctx.uc_link = &a->home_uctx;
        a->uctx.uc_stack.ss_flags = 0;
        makecontext(&a->uctx,
            (void(*)(void))actor->type->dispatch, 3, ctx, a, msg);
        int ret = swapcontext(&a->home_uctx, &a->uctx);
        assert(ret == 0);
        return !has_flag(actor, FLAG_UNSCHEDULED);
#else
        actor->type->dispatch(ctx, actor, msg);
#endif
      } else {
        actor->type->dispatch((void*)*ctx, actor, msg);
      }
      return true;
    }
  }
}

static void try_gc(pony_ctx_t* ctx, pony_actor_t* actor)
{
  if (gc_disabled(ctx)) {
    return;
  }

  if(!ponyint_heap_startgc(&actor->heap))
    return;

  DTRACE1(GC_START, (uintptr_t)ctx->scheduler);

  ponyint_gc_mark(ctx);

  if(actor->type->trace != NULL)
    actor->type->trace(ctx, actor);

  ponyint_mark_done(ctx);
  ponyint_heap_endgc(&actor->heap);

  DTRACE1(GC_END, (uintptr_t)ctx->scheduler);
}

bool ponyint_actor_run(pony_ctx_t** ctx, pony_actor_t* actor, size_t batch)
{
  (*ctx)->current = actor;
  size_t app = 0;

  if (!has_flag(actor, FLAG_SYSTEM)) {
    if (encore_actor_run_hook((encore_actor_t *)actor)) {
      if (has_flag((pony_actor_t *)actor, FLAG_UNSCHEDULED)) {
        return false;
      }
      app++;
      try_gc(*ctx, actor);
      if (app == batch) {
        return true;
      }
    }
  }

  pony_msg_t* msg;

  while(actor->continuation != NULL)
  {
    msg = actor->continuation;
    actor->continuation = atomic_load_explicit(&msg->next,
       memory_order_relaxed);
    bool ret = handle_message(ctx, actor, msg);
    ponyint_pool_free(msg->index, msg);

    if(ret)
    {
      // If we handle an application message, try to gc.
      app++;
      try_gc(*ctx, actor);

      if(app == batch)
        return !has_flag(actor, FLAG_UNSCHEDULED);
    }
  }

  // If we have been scheduled, the head will not be marked as empty.
  pony_msg_t* head = atomic_load_explicit(&actor->q.head, memory_order_relaxed);
  while((msg = ponyint_messageq_pop(&actor->q)) != NULL)
  {
    if(handle_message(ctx, actor, msg))
    {
      // If we handle an application message, try to gc.
      app++;
      try_gc(*ctx, actor);

      if(app == batch)
        return !has_flag(actor, FLAG_UNSCHEDULED);
    }

    // Stop handling a batch if we reach the head we found when we were
    // scheduled.
    if(msg == head)
      break;
  }

  assert((*ctx)->current == actor);
  // We didn't hit our app message batch limit. We now believe our queue to be
  // empty, but we may have received further messages.
  pony_assert(app < batch);
  try_gc(*ctx, actor);

  if(has_flag(actor, FLAG_UNSCHEDULED))
  {
    // When unscheduling, don't mark the queue as empty, since we don't want
    // to get rescheduled if we receive a message.
    return false;
  }

  // If we have processed any application level messages, defer blocking.
  if(app > 0)
    return true;

  // Tell the cycle detector we are blocking. We may not actually block if a
  // message is received between now and when we try to mark our queue as
  // empty, but that's ok, we have still logically blocked.
  if(!has_flag(actor, FLAG_BLOCKED | FLAG_SYSTEM) ||
    has_flag(actor, FLAG_RC_CHANGED))
  {
    set_flag(actor, FLAG_BLOCKED);
    unset_flag(actor, FLAG_RC_CHANGED);
    ponyint_cycle_block(*ctx, actor, &actor->gc);
  }

  // Return true (i.e. reschedule immediately) if our queue isn't empty.
  return !ponyint_messageq_markempty(&actor->q);
}

void ponyint_actor_destroy(pony_actor_t* actor)
{
  pony_assert(has_flag(actor, FLAG_PENDINGDESTROY));

  // Make sure the actor being destroyed has finished marking its queue
  // as empty. Otherwise, it may spuriously see that tail and head are not
  // the same and fail to mark the queue as empty, resulting in it getting
  // rescheduled.
  pony_msg_t* head = NULL;
  do
  {
    head = atomic_load_explicit(&actor->q.head, memory_order_relaxed);
  } while(((uintptr_t)head & (uintptr_t)1) != (uintptr_t)1);

  atomic_thread_fence(memory_order_acquire);
#ifdef USE_VALGRIND
  ANNOTATE_HAPPENS_AFTER(&actor->q.head);
#endif

  ponyint_messageq_destroy(&actor->q);
  ponyint_gc_destroy(&actor->gc);
  ponyint_heap_destroy(&actor->heap);

  // Free variable sized actors correctly.
  ponyint_pool_free_size(actor->type->size, actor);
}

bool is_unscheduled(pony_actor_t* a){
  return has_flag(a, FLAG_UNSCHEDULED);
}

void unset_unscheduled(pony_actor_t* runner){
  unset_flag(runner, FLAG_UNSCHEDULED);
}

gc_t* ponyint_actor_gc(pony_actor_t* actor)
{
  return &actor->gc;
}

heap_t* ponyint_actor_heap(pony_actor_t* actor)
{
  return &actor->heap;
}

bool ponyint_actor_pendingdestroy(pony_actor_t* actor)
{
  return has_flag(actor, FLAG_PENDINGDESTROY);
}

void ponyint_actor_setpendingdestroy(pony_actor_t* actor)
{
  set_flag(actor, FLAG_PENDINGDESTROY);
}

void ponyint_actor_final(pony_ctx_t* ctx, pony_actor_t* actor)
{
  // This gets run while the cycle detector is handling a message. Set the
  // current actor before running anything.
  pony_actor_t* prev = ctx->current;
  ctx->current = actor;

  // Run the actor finaliser if it has one.
  if(actor->type->final != NULL)
    actor->type->final(actor);

  // Run all outstanding object finalisers.
  ponyint_heap_final(&actor->heap);


  // Restore the current actor.
  ctx->current = prev;
}

void ponyint_actor_sendrelease(pony_ctx_t* ctx, pony_actor_t* actor)
{
  ponyint_gc_sendrelease(ctx, &actor->gc);
}

void ponyint_actor_setsystem(pony_actor_t* actor)
{
  set_flag(actor, FLAG_SYSTEM);
}

void ponyint_actor_setnoblock(bool state)
{
  actor_noblock = state;
}

PONY_API pony_actor_t* pony_create(pony_ctx_t* ctx, pony_type_t* type)
{
  pony_assert(type != NULL);

  DTRACE1(ACTOR_ALLOC, (uintptr_t)ctx->scheduler);

  // allocate variable sized actors correctly
  pony_actor_t* actor = (pony_actor_t*)ponyint_pool_alloc_size(type->size);
  memset(actor, 0, type->size);
  actor->type = type;

  ponyint_messageq_init(&actor->q);
  ponyint_heap_init(&actor->heap);
  ponyint_gc_done(&actor->gc);

  if(actor_noblock)
    ponyint_actor_setsystem(actor);

  if(ctx->current != NULL)
  {
    // actors begin unblocked and referenced by the creating actor
    actor->gc.rc = GC_INC_MORE;
    ponyint_gc_createactor(ctx->current, actor);
  } else {
    // no creator, so the actor isn't referenced by anything
    actor->gc.rc = 0;
  }

  return actor;
}

PONY_API void ponyint_destroy(pony_actor_t* actor)
{
  // This destroys an actor immediately. If any other actor has a reference to
  // this actor, the program will likely crash. The finaliser is not called.
  ponyint_actor_setpendingdestroy(actor);
  ponyint_actor_destroy(actor);
}

PONY_API pony_msg_t* pony_alloc_msg(uint32_t index, uint32_t id)
{
  pony_msg_t* msg = (pony_msg_t*)ponyint_pool_alloc(index);
  msg->index = index;
  msg->id = id;

  return msg;
}

PONY_API pony_msg_t* pony_alloc_msg_size(size_t size, uint32_t id)
{
  return pony_alloc_msg((uint32_t)ponyint_pool_index(size), id);
}

PONY_API void pony_sendv(pony_ctx_t* ctx, pony_actor_t* to, pony_msg_t* m)
{
  DTRACE2(ACTOR_MSG_SEND, (uintptr_t)ctx->scheduler, m->id);

  if(ponyint_messageq_push(&to->q, m))
  {
    if(!has_flag(to, FLAG_UNSCHEDULED))
      ponyint_sched_add(ctx, to);
  }
}

PONY_API void pony_send(pony_ctx_t* ctx, pony_actor_t* to, uint32_t id)
{
  pony_msg_t* m = pony_alloc_msg(POOL_INDEX(sizeof(pony_msg_t)), id);
  pony_sendv(ctx, to, m);
}

PONY_API void pony_sendp(pony_ctx_t* ctx, pony_actor_t* to, uint32_t id,
  void* p)
{
  pony_msgp_t* m = (pony_msgp_t*)pony_alloc_msg(
    POOL_INDEX(sizeof(pony_msgp_t)), id);
  m->p = p;

  pony_sendv(ctx, to, &m->msg);
}

PONY_API void pony_sendi(pony_ctx_t* ctx, pony_actor_t* to, uint32_t id,
  intptr_t i)
{
  pony_msgi_t* m = (pony_msgi_t*)pony_alloc_msg(
    POOL_INDEX(sizeof(pony_msgi_t)), id);
  m->i = i;

  pony_sendv(ctx, to, &m->msg);
}

PONY_API void pony_continuation(pony_actor_t* self, pony_msg_t* m)
{
  atomic_store_explicit(&m->next, self->continuation, memory_order_relaxed);
  self->continuation = m;
}

PONY_API void* pony_alloc(pony_ctx_t* ctx, size_t size)
{
  DTRACE2(HEAP_ALLOC, (uintptr_t)ctx->scheduler, size);

  return ponyint_heap_alloc(ctx->current, &ctx->current->heap, size);
}

PONY_API void* pony_alloc_small(pony_ctx_t* ctx, uint32_t sizeclass)
{
  DTRACE2(HEAP_ALLOC, (uintptr_t)ctx->scheduler, HEAP_MIN << sizeclass);

  return ponyint_heap_alloc_small(ctx->current, &ctx->current->heap, sizeclass);
}

PONY_API void* pony_alloc_large(pony_ctx_t* ctx, size_t size)
{
  DTRACE2(HEAP_ALLOC, (uintptr_t)ctx->scheduler, size);

  return ponyint_heap_alloc_large(ctx->current, &ctx->current->heap, size);
}

PONY_API void* pony_realloc(pony_ctx_t* ctx, void* p, size_t size)
{
  DTRACE2(HEAP_ALLOC, (uintptr_t)ctx->scheduler, size);

  return ponyint_heap_realloc(ctx->current, &ctx->current->heap, p, size);
}

PONY_API void* pony_alloc_final(pony_ctx_t* ctx, size_t size)
{
  DTRACE2(HEAP_ALLOC, (uintptr_t)ctx->scheduler, size);

  return ponyint_heap_alloc_final(ctx->current, &ctx->current->heap, size);
}

void* pony_alloc_small_final(pony_ctx_t* ctx, uint32_t sizeclass)
{
  DTRACE2(HEAP_ALLOC, (uintptr_t)ctx->scheduler, HEAP_MIN << sizeclass);

  return ponyint_heap_alloc_small_final(ctx->current, &ctx->current->heap,
    sizeclass);
}

void* pony_alloc_large_final(pony_ctx_t* ctx, size_t size)
{
  DTRACE2(HEAP_ALLOC, (uintptr_t)ctx->scheduler, size);

  return ponyint_heap_alloc_large_final(ctx->current, &ctx->current->heap,
    size);
}

PONY_API void pony_triggergc(pony_actor_t* actor)
{
  actor->heap.next_gc = 0;
}

PONY_API void pony_schedule(pony_ctx_t* ctx, pony_actor_t* actor)
{
  if(!has_flag(actor, FLAG_UNSCHEDULED))
    return;

  unset_flag(actor, FLAG_UNSCHEDULED);
  ponyint_sched_add(ctx, actor);
}

PONY_API void pony_unschedule(pony_ctx_t* ctx, pony_actor_t* actor)
{
  if(has_flag(actor, FLAG_BLOCKED))
  {
    ponyint_cycle_unblock(ctx, actor);
    unset_flag(actor, FLAG_BLOCKED | FLAG_RC_CHANGED);
  }

  set_flag(actor, FLAG_UNSCHEDULED);
}

PONY_API void pony_become(pony_ctx_t* ctx, pony_actor_t* actor)
{
  ctx->current = actor;
}

bool pony_system_actor(pony_actor_t *actor)
{
  return has_flag(actor, FLAG_SYSTEM);
}

bool pony_reschedule(pony_actor_t *actor)
{
  return !has_flag(actor, FLAG_UNSCHEDULED);
}

// NOTE: (Kiko) The function `pony_poll` is not used by Encore and/or the
// PonyRT. This means that it is used by the Pony compiler. In the Encore
// context, if we allow this function, we would need to pass a `pony_ctx_t**`
// as argument, since the `actor_run` requires a `pony_ctx_t**`. The reason for
// this is that the `actor_run` updates the `ctx` argument to point to a new
// context and this can only be done with a pointer to a pointer.
// void pony_poll(pony_ctx_t* ctx)
// {
//   assert(ctx->current != NULL);
//   ponyint_actor_run(ctx, ctx->current, 1);
// }
