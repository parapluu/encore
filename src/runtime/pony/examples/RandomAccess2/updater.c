#define __STDC_FORMAT_MACROS
#include <pony/pony.h>
#include "arrays.h"
#include "main.h"
#include "updater.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <assert.h>

#define POLY 7L
#define PERIOD 1317624576693539401L

typedef struct updater_t
{
  pony_actor_t* main;
  array_actor_t* updaters;
  uint64_t index;
  uint64_t size;
  uint64_t chunk;
  uint64_t rnd;
  uint64_t shift;
  uint64_t mask;
  uint64_t* table;
} updater_t;

enum
{
  MSG_CREATE,
  MSG_APPLY,
  MSG_RECEIVE,
  MSG_DONE,
};

static void trace(void* p);
static pony_msg_t* message_type(uint64_t id);
static void dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv);

static pony_actor_type_t type =
{
  2,
  {sizeof(updater_t), trace, NULL, NULL},
  message_type,
  dispatch,
  NULL
};

static pony_msg_t m_create = {6, {PONY_ACTOR, &array_actor_type, PONY_NONE}};
static pony_msg_t m_apply = {1, {PONY_NONE}};
static pony_msg_t m_receive = {1, {&array_uint64_type}};
static pony_msg_t m_done = {0, {PONY_NONE}};

static void trace(void* p)
{
  updater_t* this = p;

  pony_traceactor(this->main);
  pony_traceobject(this->updaters, array_actor_trace);
  pony_trace(this->table);
}

static pony_msg_t* message_type(uint64_t id)
{
  switch(id)
  {
    case MSG_CREATE: return &m_create;
    case MSG_APPLY: return &m_apply;
    case MSG_RECEIVE: return &m_receive;
    case MSG_DONE: return &m_done;
  }

  return NULL;
}

static uint64_t rng_next(int64_t n)
{
  return (n << 1) ^ ((n < 0) ? POLY : 0);
}

static uint64_t rng_seed(int64_t n)
{
  n = n % PERIOD;
  if(n < 0) n += PERIOD;
  if(n == 0) return 1;

  uint64_t m2[64];
  uint64_t temp = 1;

  for(int i = 0; i < 64; i++)
  {
    m2[i] = temp;
    temp = rng_next(rng_next(temp));
  }

  int i = 62;

  while(i >= 0)
  {
    if((n >> i) & 1) break;
    i--;
  }

  uint64_t ran = 2;

  while(i > 0)
  {
    temp = 0;

    for(int j = 0; j < 64; j++)
    {
      if((ran >> j) & 1) temp ^= m2[j];
    }

    ran = temp;
    i--;

    if((n >> i) & 1) ran = rng_next(ran);
  }

  return ran;
}

static void dispatch(pony_actor_t* actor, void* p, uint64_t id, int argc, pony_arg_t* argv)
{
  updater_t* this = p;

  switch(id)
  {
    case MSG_CREATE:
    {
      this = pony_alloc(sizeof(updater_t));
      pony_set(this);

      this->main = argv[0].p;
      this->updaters = argv[1].p;
      this->index = argv[2].i;
      this->size = argv[3].i;
      this->chunk = argv[4].i;
      this->rnd = rng_seed(argv[4].i);

      this->shift = __builtin_ffsl(this->size) - 1;
      this->mask = this->updaters->count - 1;

      uint64_t offset = this->index * this->size;
      this->table = pony_alloc(this->size * sizeof(uint64_t));

      for(uint64_t i = 0; i < this->size; i++)
        this->table[i] = i + offset;
      break;
    }

    case MSG_APPLY:
    {
      const uint64_t iterate = argv[0].i;
      const uint64_t me = this->index;
      const uint64_t mask = this->size - 1;
      const uint64_t count = this->updaters->count;
      array_uint64_t* list[count];

      for(uint64_t i = 0; i < count; i++)
      {
        if(i != me)
        {
          // FIX: should be in array_uint64_t
          list[i] = pony_alloc(sizeof(array_uint64_t));
          list[i]->count = 0;
          list[i]->array = pony_alloc(this->chunk * sizeof(uint64_t));
        }
      }

      const uint64_t chunk = this->chunk;
      uint64_t datum = this->rnd;

      for(uint64_t i = 0; i < chunk; i++)
      {
        datum = rng_next(datum);
        const uint64_t updater = (datum >> this->shift) & this->mask;

        if(updater == me)
        {
          const uint64_t index = datum & mask;
          this->table[index] ^= datum;
        } else {
          const uint64_t index = list[updater]->count++;
          list[updater]->array[index] = datum;
        }
      }

      this->rnd = datum;

      for(uint64_t i = 0; i < count; i++)
      {
        if((i != me) && (list[i]->count > 0))
          updater_receive(this->updaters->array[i], list[i]);
      }

      if(iterate > 0)
      {
        updater_apply(actor, iterate - 1);
      } else {
        main_done(this->main);
      }
      break;
    }

    case MSG_RECEIVE:
    {
      array_uint64_t* data = argv[0].p;
      uint64_t mask = this->size - 1;

      for(uint64_t i = 0; i < data->count; i++)
      {
        uint64_t datum = data->array[i];
        uint64_t index = datum & mask;
        this->table[index] ^= datum;
      }
      break;
    }

    case MSG_DONE:
    {
      main_conf(this->main);
      break;
    }
  }
}

pony_actor_t* updater_create(pony_actor_t* main, array_actor_t* updaters,
  uint64_t index, uint64_t size, uint64_t chunk, uint64_t seed)
{
  pony_actor_t* updater = pony_create(&type);

  pony_arg_t argv[6];
  argv[0].p = main;
  argv[1].p = updaters;
  argv[2].i = index;
  argv[3].i = size;
  argv[4].i = chunk;
  argv[5].i = seed;
  pony_sendv(updater, MSG_CREATE, 6, argv);

  return updater;
}

void updater_apply(pony_actor_t* updater, uint64_t iterate)
{
  pony_sendi(updater, MSG_APPLY, iterate);
}

void updater_receive(pony_actor_t* updater, array_uint64_t* data)
{
  pony_sendp(updater, MSG_RECEIVE, data);
}

void updater_done(pony_actor_t* updater)
{
  pony_send(updater, MSG_DONE);
}
