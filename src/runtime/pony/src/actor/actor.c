#define _XOPEN_SOURCE 800
#include "ucontext.h"
#include "actor.h"
#include "messageq.h"
#include "../sched/scheduler.h"
#include "../mem/pool.h"
#include "../gc/cycle.h"
#include "../gc/trace.h"
#include <string.h>
#include <stdio.h>
#include <assert.h>

#include "encore.h"
enum
{
  FLAG_BLOCKED = 1 << 0,
  FLAG_SYSTEM = 1 << 1,
  FLAG_UNSCHEDULED = 1 << 2,
  FLAG_PENDINGDESTROY = 1 << 3,
};

struct pony_actor_t
{
  pony_type_t* type;
  messageq_t q;
  pony_msg_t* continuation;
  struct pony_actor_t* dormant_next;

  // keep things accessed by other actors on a separate cache line
  __pony_spec_align__(heap_t heap, 64);
  gc_t gc;
  struct pony_actor_t* next;
  uint8_t flags;
};

static __pony_thread_local pony_actor_t* this_actor;

bool has_flag(pony_actor_t* actor, uint8_t flag)
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

static bool handle_message(pony_actor_t* actor, pony_msg_t* msg)
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
      // if we are blocked and our rc changes, block again
      pony_msgp_t* m = (pony_msgp_t*)msg;

      if(gc_acquire(&actor->gc, (actorref_t*)m->p) &&
        has_flag(actor, FLAG_BLOCKED))
      {
        cycle_block(actor, &actor->gc);
      }

      return false;
    }

    case ACTORMSG_RELEASE:
    {
      // if we are blocked and our rc changes, block again
      pony_msgp_t* m = (pony_msgp_t*)msg;

      if(gc_release(&actor->gc, (actorref_t*)m->p) &&
        has_flag(actor, FLAG_BLOCKED))
      {
        cycle_block(actor, &actor->gc);
      }

      return false;
    }

    case ACTORMSG_CONF:
    {
      pony_msgi_t* m = (pony_msgi_t*)msg;
      cycle_ack((size_t)m->i);
      return false;
    }

    default:
    {
      if(has_flag(actor, FLAG_BLOCKED))
      {
        // if we are blocked and we get an application message, unblock
        cycle_unblock(actor);
        unset_flag(actor, FLAG_BLOCKED);
      }

#ifndef LAZY_IMPL
      if (!has_flag(actor, FLAG_SYSTEM)) {
      // if (0) {
        encore_actor_t *a = (encore_actor_t *)actor;
        getcontext(&a->ctx);
        a->ctx.uc_stack.ss_sp = get_local_page_stack();
        a->ctx.uc_stack.ss_size = Stack_Size;
        a->ctx.uc_link = &a->home_ctx;
        a->ctx.uc_stack.ss_flags = 0;
        makecontext(&a->ctx, (void(*)(void))actor->type->dispatch, 2, a, msg);
        int ret = swapcontext(&a->home_ctx, &a->ctx);
        assert(ret == 0);
      } else {
        actor->type->dispatch(actor, msg);
      }
#else
      actor->type->dispatch(actor, msg);
#endif

      return true;
    }
  }
}

bool actor_run(pony_actor_t* actor)
{
  pony_msg_t* msg;
  this_actor = actor;

  if (!has_flag(actor, FLAG_SYSTEM)) {
    if (encore_actor_run_hook((encore_actor_t *)actor)) {
      return !has_flag((pony_actor_t *)actor, FLAG_UNSCHEDULED);
    }
  }

  // if(1)
  if(heap_startgc(&actor->heap))
  {
    if(actor->type->trace != NULL)
    {
      pony_gc_mark();
      actor->type->trace(actor);
    }

    gc_mark(&actor->gc);
    gc_sweep(&actor->gc);
    gc_done(&actor->gc);
    heap_endgc(&actor->heap);
  }

  if(actor->continuation != NULL)
  {
    msg = actor->continuation;
    actor->continuation = NULL;
    bool ret = handle_message(actor, msg);
    pool_free(msg->index, msg);

    if(ret)
      return !has_flag(actor, FLAG_UNSCHEDULED);
  }

  while((msg = messageq_pop(&actor->q)) != NULL)
  {
    if(handle_message(actor, msg)) {
      return !has_flag(actor, FLAG_UNSCHEDULED);
    }
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

bool actor_emptyqueue(pony_actor_t* actor)
{
  return messageq_markempty(&actor->q);
}

void actor_destroy(pony_actor_t* actor)
{
  assert(has_flag(actor, FLAG_PENDINGDESTROY));

  messageq_destroy(&actor->q);
  heap_destroy(&actor->heap);

  // free variable sized actors correctly
  pool_free_size(actor->type->size, actor);
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
  return actor->type->final != NULL;
}

void actor_final(pony_actor_t* actor)
{
  actor->type->final(actor);
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

pony_actor_t* pony_create(pony_type_t* type)
{
  assert(type != NULL);

  // allocate variable sized actors correctly
  pony_actor_t* actor = (pony_actor_t*)pool_alloc_size(type->size);
  memset(actor, 0, type->size);
  actor->type = type;

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

  return actor;
}

pony_msg_t* pony_alloc_msg(uint32_t index, uint32_t id)
{
  pony_msg_t* msg = (pony_msg_t*)pool_alloc(index);
  msg->index = index;
  msg->id = id;

  return msg;
}

void pony_sendv(pony_actor_t* to, pony_msg_t* m)
{
  if(messageq_push(&to->q, m))
  {
    if(!has_flag(to, FLAG_UNSCHEDULED))
      scheduler_add(to);
  }
}

void pony_send(pony_actor_t* to, uint32_t id)
{
  pony_msg_t* m = pony_alloc_msg(0, id);
  pony_sendv(to, m);
}

void pony_sendp(pony_actor_t* to, uint32_t id, void* p)
{
  pony_msgp_t* m = (pony_msgp_t*)pony_alloc_msg(0, id);
  m->p = p;

  pony_sendv(to, &m->msg);
}

void pony_sendi(pony_actor_t* to, uint32_t id, intptr_t i)
{
  pony_msgi_t* m = (pony_msgi_t*)pony_alloc_msg(0, id);
  m->i = i;

  pony_sendv(to, &m->msg);
}

void pony_sendd(pony_actor_t* to, uint32_t id, double d)
{
  pony_msgd_t* m = (pony_msgd_t*)pony_alloc_msg(0, id);
  m->d = d;

  pony_sendv(to, &m->msg);
}

void pony_sendargs(pony_actor_t* to, uint32_t id, int argc, char** argv)
{
  pony_main_msg_t* m = (pony_main_msg_t*)pony_alloc_msg(0, id);
  m->argc = argc;
  m->argv = argv;

  pony_sendv(to, &m->msg);
}

void pony_continuation(pony_actor_t* to, pony_msg_t* m)
{
  assert(to->continuation == NULL);
  m->next = NULL;
  to->continuation = m;
}

void* pony_alloc(size_t size)
{
  return heap_alloc(this_actor, &this_actor->heap, size);
}

void* pony_realloc(void* p, size_t size)
{
  return heap_realloc(this_actor, &this_actor->heap, p, size);
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

void pony_schedule_first(pony_actor_t* actor)
{
  if(!has_flag(actor, FLAG_UNSCHEDULED))
    return;

  unset_flag(actor, FLAG_UNSCHEDULED);
  scheduler_add_first(actor);
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
