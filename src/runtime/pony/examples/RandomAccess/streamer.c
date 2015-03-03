#include "streamer.h"
#include "updater.h"
#include "main.h"
#include "arrays.h"
#include <string.h>
#include <assert.h>

#define POLY 7L
#define PERIOD 1317624576693539401L

typedef struct streamer_t
{
  pony_actor_pad_t pad;

  pony_actor_t* main;
  array_actor_t* updaters;
  uint64_t shift;
  uint64_t mask;
  uint64_t chunk;
  uint64_t rnd;
} streamer_t;

typedef struct create_msg_t
{
  pony_msg_t msg;
  pony_actor_t* main;
  array_actor_t* updaters;
  uint64_t size;
  uint64_t chunk;
  uint64_t seed;
} create_msg_t;

enum
{
  MSG_CREATE,
  MSG_APPLY
};

static void trace_streamer(void* p);
static void do_streamer(pony_actor_t* self, pony_msg_t* msg);

static pony_type_t type =
{
  2,
  sizeof(streamer_t),
  trace_streamer,
  NULL,
  NULL,
  do_streamer,
  NULL
};

static void trace_streamer(void* p)
{
  streamer_t* self = (streamer_t*)p;

  pony_traceactor(self->main);
  pony_traceobject(self->updaters, array_actor_trace);
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

static void do_streamer(pony_actor_t* actor, pony_msg_t* msg)
{
  streamer_t* self = (streamer_t*)actor;

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
      self->shift = __pony_ffsl(m->size) - 1;
      self->mask = self->updaters->count - 1;
      self->chunk = m->chunk;
      self->rnd = rng_seed(m->seed);
      break;
    }

    case MSG_APPLY:
    {
      pony_msgi_t* m = (pony_msgi_t*)msg;
      const uint64_t iterate = m->i;
      const uint64_t chunk = self->chunk;
      const uint64_t count = self->updaters->count;
      VLA(uint64_t*, list, count);
      VLA(uint64_t, size, count);

      memset(size, 0, count * sizeof(uint64_t));

      for(uint64_t i = 0; i < count; i++)
        list[i] = (uint64_t*)pony_alloc(chunk * sizeof(uint64_t));

      uint64_t datum = self->rnd;
      const uint64_t shift = self->shift;
      const uint64_t mask = self->mask;

      for(uint64_t i = 0; i < chunk; i++)
      {
        datum = rng_next(datum);
        const uint64_t updater = (datum >> shift) & mask;
        assert(updater < count);
        const uint64_t index = size[updater]++;
        assert(index < chunk);
        list[updater][index] = datum;
      }

      self->rnd = datum;
      pony_actor_t** cur = self->updaters->array;

      for(int i = 0; i < count; i++)
      {
        if(size[i] > 0)
          updater_apply(*cur, size[i], list[i]);

        cur++;
      }

      if(iterate > 0)
      {
        streamer_apply(actor, iterate - 1);
      } else {
        main_streamerdone(self->main);
      }
      break;
    }
  }
}

pony_actor_t* streamer_create(pony_actor_t* main, array_actor_t* updaters,
  uint64_t size, uint64_t chunk, uint64_t seed)
{
  pony_actor_t* streamer = pony_create(&type);

  pony_gc_send();
  pony_traceactor(main);
  pony_traceobject(updaters, array_actor_trace);
  pony_send_done();

  create_msg_t* m = (create_msg_t*)pony_alloc_msg(0, MSG_CREATE);
  m->main = main;
  m->updaters = updaters;
  m->size = size;
  m->chunk = chunk;
  m->seed = seed;

  pony_sendv(streamer, &m->msg);
  return streamer;
}

void streamer_apply(pony_actor_t* streamer, uint64_t iterate)
{
  pony_sendi(streamer, MSG_APPLY, iterate);
}
