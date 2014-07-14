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
  pony_actor_t* main;
  array_actor_t* updaters;
  uint64_t shift;
  uint64_t mask;
  uint64_t chunk;
  uint64_t rnd;
} streamer_t;

enum
{
  MSG_CREATE,
  MSG_APPLY
};

static void trace_streamer(void* p);
static pony_msg_t* msg_streamer(uint64_t id);
static void do_streamer(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv);

static pony_actor_type_t type =
{
  2,
  {sizeof(streamer_t), trace_streamer, NULL, NULL},
  msg_streamer,
  do_streamer,
  NULL
};

static pony_msg_t m_create = {5, {PONY_ACTOR, &array_actor_type, PONY_NONE}};
static pony_msg_t m_apply = {1, {PONY_NONE}};

static void trace_streamer(void* p)
{
  streamer_t* this = p;

  pony_traceactor(this->main);
  pony_traceobject(this->updaters, array_actor_trace);
}

static pony_msg_t* msg_streamer(uint64_t id)
{
  switch(id)
  {
    case MSG_CREATE: return &m_create;
    case MSG_APPLY: return &m_apply;
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

static void do_streamer(pony_actor_t* actor, void* p, uint64_t id, int argc, pony_arg_t* argv)
{
  streamer_t* this = p;

  switch(id)
  {
    case MSG_CREATE:
    {
      this = pony_alloc(sizeof(streamer_t));
      pony_set(this);

      this->main = argv[0].p;
      this->updaters = argv[1].p;
      this->shift = __builtin_ffsl(argv[2].i) - 1;
      this->mask = this->updaters->count - 1;
      this->chunk = argv[3].i;
      this->rnd = rng_seed(argv[4].i);
      break;
    }

    case MSG_APPLY:
    {
      const uint64_t iterate = argv[0].i;
      const uint64_t chunk = this->chunk;
      const uint64_t count = this->updaters->count;
      uint64_t* list[count];
      uint64_t size[count];

      memset(size, 0, count * sizeof(uint64_t));

      for(uint64_t i = 0; i < count; i++)
        list[i] = pony_alloc(chunk * sizeof(uint64_t));

      uint64_t datum = this->rnd;
      const uint64_t shift = this->shift;
      const uint64_t mask = this->mask;

      for(uint64_t i = 0; i < chunk; i++)
      {
        datum = rng_next(datum);
        const uint64_t updater = (datum >> shift) & mask;
        assert(updater < count);
        const uint64_t index = size[updater]++;
        assert(index < chunk);
        list[updater][index] = datum;
      }

      this->rnd = datum;
      pony_actor_t** cur = this->updaters->array;

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
        main_streamerdone(this->main);
      }
      break;
    }
  }
}

pony_actor_t* streamer_create(pony_actor_t* main, array_actor_t* updaters,
  uint64_t size, uint64_t chunk, uint64_t seed)
{
  pony_actor_t* streamer = pony_create(&type);

  pony_arg_t argv[5];
  argv[0].p = main;
  argv[1].p = updaters;
  argv[2].i = size;
  argv[3].i = chunk;
  argv[4].i = seed;
  pony_sendv(streamer, MSG_CREATE, 5, argv);

  return streamer;
}

void streamer_apply(pony_actor_t* streamer, uint64_t iterate)
{
  pony_sendi(streamer, MSG_APPLY, iterate);
}
