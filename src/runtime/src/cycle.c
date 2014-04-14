#define __STDC_FORMAT_MACROS
#include "cycle.h"
#include "scheduler.h"
#include "actor.h"
#include "map.h"
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <assert.h>

#define CYCLE_MIN_DEFERRED 1024
#define CYCLE_MAX_DEFERRED (1 << 20)

enum
{
  CYCLE_INIT,
  CYCLE_BLOCK,
  CYCLE_UNBLOCK,
  CYCLE_UPDATERC,
  CYCLE_ACK,
  CYCLE_FINISH
};

typedef struct perceived_t
{
  uint64_t token;
  uint64_t ack_remaining;
  map_t* map;
  struct perceived_t* next;
} perceived_t;

typedef struct detector_t
{
  uint64_t next_token;
  uint32_t next_deferred;
  uint32_t count_deferred;
  map_t* blocked;
  perceived_t* perceived;

  uint64_t block_msgs;
  uint64_t unblock_msgs;
  uint64_t updaterc_msgs;
  uint64_t conf_msgs;
  uint64_t ack_msgs;
  uint64_t attempted;
  uint64_t detected;
  uint64_t collected;

  bool finished;
  bool stats;
} detector_t;

static pony_actor_t* cycle_detector;

static void perceived_trace(void* p)
{
  perceived_t* this = p;
  pony_trace(&this->next, perceived_trace, sizeof(perceived_t), PONY_MUTABLE);
}

static void detector_trace(void* p)
{
  detector_t* this = p;
  pony_trace(&this->perceived, perceived_trace, sizeof(perceived_t),
    PONY_MUTABLE);
}

static void ack_token(detector_t* d, uint64_t token)
{
  perceived_t* p = d->perceived;
  perceived_t* last = NULL;

  while(p != NULL)
  {
    if(p->token == token)
    {
      assert(p->ack_remaining > 0);
      p->ack_remaining--;

      if(p->ack_remaining == 0)
      {
        if(last != NULL)
        {
          last->next = p->next;
        } else {
          d->perceived = p->next;
        }

        map_collectcycle(p->map);
        map_removecycle(d->blocked, p->map);
        map_free(p->map, false);
        d->collected++;
      }

      return;
    }

    last = p;
    p = p->next;
  }
}

static void expire_cycles(detector_t* d, pony_actor_t* actor)
{
  perceived_t* p = d->perceived;
  perceived_t* next;
  d->perceived = NULL;

  while(p != NULL)
  {
    next = p->next;

    if(map_contains(p->map, actor))
    {
      map_free(p->map, false);
    } else {
      p->next = d->perceived;
      d->perceived = p;
    }

    p = next;
  }
}

static void detect_cycle(void* arg, map_t* cycle)
{
  detector_t* d = arg;

  perceived_t* p = pony_alloc(sizeof(perceived_t));
  p->token = d->next_token++;
  p->ack_remaining = map_size(cycle);
  p->map = cycle;
  p->next = d->perceived;
  d->perceived = p;
  d->detected++;
  d->conf_msgs += p->ack_remaining;
}

static void deferred_detect(detector_t* d)
{
  if(++d->count_deferred < d->next_deferred) return;
  d->count_deferred = 0;

  if(map_detectcycles(d->blocked, detect_cycle, d, d->next_token))
  {
    if(d->next_deferred > CYCLE_MIN_DEFERRED)
    {
      d->next_deferred >>= 1;
    }
  } else {
    d->attempted++;

    if(d->next_deferred < CYCLE_MAX_DEFERRED)
    {
      d->next_deferred <<= 1;
    }
  }
}

static void print_stats(detector_t* d)
{
  printf("Cycle detector:\n"
    "\tblock_msgs: %"PRIu64"\n"
    "\tunblock_msgs: %"PRIu64"\n"
    "\tupdaterc_msgs: %"PRIu64"\n"
    "\tconf_msgs: %"PRIu64"\n"
    "\tack_msgs: %"PRIu64"\n"
    "\tattempted: %"PRIu64"\n"
    "\tdetected: %"PRIu64"\n"
    "\tcollected: %"PRIu64"\n",
    d->block_msgs,
    d->unblock_msgs,
    d->updaterc_msgs,
    d->conf_msgs,
    d->ack_msgs,
    d->attempted,
    d->detected,
    d->collected
    );
}

static pony_msg_t m_cycle_init = {0, {{NULL, 0, PONY_PRIMITIVE}}};
static pony_msg_t m_cycle_block = {3, {{NULL, 0, PONY_PRIMITIVE}}};
static pony_msg_t m_cycle_unblock = {1, {{NULL, 0, PONY_PRIMITIVE}}};
static pony_msg_t m_cycle_updaterc = {2, {{NULL, 0, PONY_PRIMITIVE}}};
static pony_msg_t m_cycle_ack = {1, {{NULL, 0, PONY_PRIMITIVE}}};
static pony_msg_t m_cycle_finish = {1, {{NULL, 0, PONY_PRIMITIVE}}};
static pony_msg_t m_cycle_stats = {0, {{NULL, 0, PONY_PRIMITIVE}}};

static pony_msg_t* message_type(uint64_t id)
{
  switch(id)
  {
    case CYCLE_INIT: return &m_cycle_init;
    case CYCLE_BLOCK: return &m_cycle_block;
    case CYCLE_UNBLOCK: return &m_cycle_unblock;
    case CYCLE_UPDATERC: return &m_cycle_updaterc;
    case CYCLE_ACK: return &m_cycle_ack;
    case CYCLE_FINISH: return &m_cycle_finish;
  }

  return NULL;
}

static void dispatch(pony_actor_t* this, void* p, uint64_t id,
  int argc, pony_arg_t* argv)
{
  detector_t* d = p;

  switch(id)
  {
    case CYCLE_INIT:
      d = pony_alloc(sizeof(detector_t));
      memset(d, 0, sizeof(detector_t));
      d->next_deferred = CYCLE_MIN_DEFERRED;
      pony_set(d);
      break;

    case CYCLE_BLOCK:
      d->block_msgs++;
      map_block(&d->blocked, argv[0].p, argv[1].i, argv[2].p);
      deferred_detect(d);
      break;

    case CYCLE_UNBLOCK:
      d->unblock_msgs++;
      map_unblock(d->blocked, argv[0].p);
      expire_cycles(d, argv[0].p);
      break;

    case CYCLE_UPDATERC:
      d->updaterc_msgs++;
      map_updaterc(d->blocked, argv[0].p, argv[1].i);
      expire_cycles(d, argv[0].p);
      deferred_detect(d);
      break;

    case CYCLE_ACK:
      d->ack_msgs++;
      ack_token(d, argv[0].i);

      if(d->finished && (d->perceived == NULL))
      {
        if(d->stats) print_stats(d);
        scheduler_terminate();
      }
      break;

    case CYCLE_FINISH:
      d->finished = true;
      d->stats = argv[1].i != 0;

      if(argv[0].i != 0)
      {
        while(map_detectcycles(d->blocked, detect_cycle, d, d->next_token));
      }

      if((argv[0].i == 0) || (d->perceived == NULL))
      {
        if(d->stats) print_stats(d);
        scheduler_terminate();
      }
      break;
  }
}

static pony_actor_type_t type =
{
  0,
  {detector_trace, sizeof(detector_t), PONY_ACTOR},
  message_type,
  dispatch
};

void cycle_create()
{
  cycle_detector = actor_create(&type);
  pony_send(cycle_detector, CYCLE_INIT);
}

void cycle_block(pony_actor_t* actor, uint32_t rc, map_t* ref)
{
  pony_arg_t argv[3];
  argv[0].p = actor;
  argv[1].i = rc;
  argv[2].p = ref;

  pony_sendv(cycle_detector, CYCLE_BLOCK, 3, argv);
}

void cycle_unblock(pony_actor_t* actor)
{
  pony_sendp(cycle_detector, CYCLE_UNBLOCK, actor);
}

void cycle_updaterc(pony_actor_t* actor, uint32_t rc)
{
  pony_arg_t argv[2];
  argv[0].p = actor;
  argv[1].i = rc;

  pony_sendv(cycle_detector, CYCLE_UPDATERC, 2, argv);
}

void cycle_ack(uint64_t token)
{
  pony_sendi(cycle_detector, CYCLE_ACK, token);
}

void cycle_finish(bool finish, bool stats)
{
  pony_arg_t argv[2];
  argv[0].i = finish;
  argv[1].i = stats;
  actor_sendv(cycle_detector, CYCLE_FINISH, 2, argv);
}
