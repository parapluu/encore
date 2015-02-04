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
  pony_actor_pad_t pad;
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

typedef struct create_msg_t
{
  pony_msg_t msg;
  pony_actor_t* main;
  array_actor_t* updaters;
  uint64_t index;
  uint64_t size;
  uint64_t chunk;
  uint64_t seed;
} create_msg_t;

enum
{
  MSG_CREATE,
  MSG_APPLY,
  MSG_RECEIVE,
  MSG_DONE,
};

static void trace(void* p);
static void dispatch(pony_actor_t* self, pony_msg_t* msg);

static pony_type_t type =
{
  3,
  sizeof(updater_t),
  trace,
  NULL,
  NULL,
  dispatch,
  NULL
};

static void trace(void* p)
{
  updater_t* self = (updater_t*)p;

  pony_traceactor(self->main);
  pony_traceobject(self->updaters, array_actor_trace);
  pony_trace(self->table);
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

  for(uint32_t i = 0; i < 64; i++)
  {
    m2[i] = temp;
    temp = rng_next(rng_next(temp));
  }

  int32_t j = 62;

  while(j >= 0)
  {
    if((n >> j) & 1) break;
    j--;
  }

  uint64_t ran = 2;

  while(j > 0)
  {
    temp = 0;

    for(uint32_t k = 0; k < 64; k++)
    {
      if((ran >> k) & 1) temp ^= m2[k];
    }

    ran = temp;
    j--;

    if((n >> j) & 1) ran = rng_next(ran);
  }

  return ran;
}

static void dispatch(pony_actor_t* actor, pony_msg_t* msg)
{
  updater_t* self = (updater_t*)actor;

  switch(msg->id)
  {
    case MSG_CREATE:
    {
      create_msg_t* m = (create_msg_t*)msg;

      pony_gc_recv();
      pony_traceactor(m->main);
      pony_traceobject(m->updaters, array_actor_trace);
      pony_recv_done();

      self->main = m->main;
      self->updaters = m->updaters;
      self->index = m->index;
      self->size = m->size;
      self->chunk = m->chunk;
      self->rnd = rng_seed(m->seed);

      self->shift = __pony_ffsl(self->size) - 1;
      self->mask = self->updaters->count - 1;

      uint64_t offset = self->index * self->size;
      self->table = (uint64_t*)pony_alloc(self->size * sizeof(uint64_t));

      for(uint64_t i = 0; i < self->size; i++)
        self->table[i] = i + offset;
      break;
    }

    case MSG_APPLY:
    {
      pony_msgi_t* m = (pony_msgi_t*)msg;

      const uint64_t iterate = m->i;
      const uint64_t me = self->index;
      const uint64_t mask = self->size - 1;
      const uint64_t count = self->updaters->count;
      VLA(array_uint64_t*, list, count);

      for(uint64_t i = 0; i < count; i++)
      {
        if(i != me)
        {
          // TODO: should be in array_uint64_t
          list[i] = (array_uint64_t*)pony_alloc(sizeof(array_uint64_t));
          list[i]->count = 0;
          list[i]->array = (uint64_t*)pony_alloc(
            self->chunk * sizeof(uint64_t));
        }
      }

      const uint64_t chunk = self->chunk;
      uint64_t datum = self->rnd;

      for(uint64_t i = 0; i < chunk; i++)
      {
        datum = rng_next(datum);
        const uint64_t updater = (datum >> self->shift) & self->mask;

        if(updater == me)
        {
          const uint64_t index = datum & mask;
          self->table[index] ^= datum;
        } else {
          const uint64_t index = list[updater]->count++;
          list[updater]->array[index] = datum;
        }
      }

      self->rnd = datum;

      for(uint64_t i = 0; i < count; i++)
      {
        if((i != me) && (list[i]->count > 0))
          updater_receive(self->updaters->array[i], list[i]);
      }

      if(iterate > 0)
      {
        updater_apply(actor, iterate - 1);
      } else {
        main_done(self->main);
      }
      break;
    }

    case MSG_RECEIVE:
    {
      pony_msgp_t* m = (pony_msgp_t*)msg;
      array_uint64_t* data = (array_uint64_t*)m->p;

      pony_gc_recv();
      pony_traceobject(data, array_uint64_trace);
      pony_recv_done();

      uint64_t mask = self->size - 1;

      for(uint64_t i = 0; i < data->count; i++)
      {
        uint64_t datum = data->array[i];
        uint64_t index = datum & mask;
        self->table[index] ^= datum;
      }
      break;
    }

    case MSG_DONE:
    {
      main_conf(self->main);
      break;
    }
  }
}

pony_actor_t* updater_create(pony_actor_t* main, array_actor_t* updaters,
  uint64_t index, uint64_t size, uint64_t chunk, uint64_t seed)
{
  pony_actor_t* updater = pony_create(&type);

  pony_gc_send();
  pony_traceactor(main);
  pony_traceobject(updaters, array_actor_trace);
  pony_send_done();

  create_msg_t* m = (create_msg_t*)pony_alloc_msg(0, MSG_CREATE);
  m->main = main;
  m->updaters = updaters;
  m->index = index;
  m->size = size;
  m->chunk = chunk;
  m->seed = seed;

  pony_sendv(updater, &m->msg);
  return updater;
}

void updater_apply(pony_actor_t* updater, uint64_t iterate)
{
  pony_sendi(updater, MSG_APPLY, iterate);
}

void updater_receive(pony_actor_t* updater, array_uint64_t* data)
{
  pony_gc_send();
  pony_traceobject(data, array_uint64_trace);
  pony_send_done();

  pony_sendp(updater, MSG_RECEIVE, data);
}

void updater_done(pony_actor_t* updater)
{
  pony_send(updater, MSG_DONE);
}
