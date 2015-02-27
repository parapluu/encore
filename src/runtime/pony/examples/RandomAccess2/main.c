#include <pony/pony.h>
#include <stdint.h>
#include <stdbool.h>

#include "main.h"
#include "arrays.h"
#include "updater.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <platform/platform.h>

typedef struct ring_t
{
  pony_actor_pad_t pad;
  uint64_t done_count;
  uint64_t conf_count;
  uint64_t updates;
  array_actor_t* updaters;
  struct timeval start;
} main_t;

enum
{
  MSG_ARGS,
  MSG_DONE,
  MSG_CONF
};

static void trace(void* p);
static void dispatch(pony_actor_t* self, pony_msg_t* msg);

static pony_type_t type =
{
  2,
  sizeof(main_t),
  trace,
  NULL,
  NULL,
  dispatch,
  NULL
};

static void trace(void* p)
{
  main_t* self = (main_t*)p;
  pony_traceobject(self->updaters, array_actor_trace);
}

static void usage()
{
  printf(
    "gups OPTIONS\n"
    "  --logtable N   log2 of the total table size (def 20)\n"
    "  --iterate N    number of iterations (def 10000)\n"
    "  --chunk N      chunk size (def 1024)\n"
    "  --actors N     number of actors (def 4)\n"
    );
}

static uint64_t next_pow2(uint64_t v)
{
  v--;
  v |= v >> 1;
  v |= v >> 2;
  v |= v >> 4;
  v |= v >> 8;
  v |= v >> 16;
  v |= v >> 32;
  return v + 1;
}

static bool getarg_u64(const char* name, char** argv, uint64_t* val)
{
  if(!strcmp(argv[0], name))
  {
    if(argv[1] != NULL)
    {
      *val = atoi(argv[1]);
    } else {
      *val = 0;
    }

    return true;
  }

  return false;
}

static void dispatch(pony_actor_t* actor, pony_msg_t* msg)
{
  main_t* self = (main_t*)actor;

  switch(msg->id)
  {
    case MSG_ARGS:
    {
      pony_main_msg_t* m = (pony_main_msg_t*)msg;

      self->done_count = 0;
      self->conf_count = 0;

      uint64_t logtable = 20;
      uint64_t iterate = 10000;
      uint64_t chunk = 1024;
      uint64_t actor_count = 4;

      for(int i = 1; i < m->argc; i++)
      {
        if(getarg_u64("--logtable", &m->argv[i], &logtable) ||
          getarg_u64("--iterate", &m->argv[i], &iterate) ||
          getarg_u64("--chunk", &m->argv[i], &chunk) ||
          getarg_u64("--actors", &m->argv[i], &actor_count)
          )
        {
          i++;
        } else {
          usage();
          return;
        }
      }

      actor_count = next_pow2(actor_count);
      uint64_t size = ((uint64_t)1 << logtable) / actor_count;
      self->updates = chunk * iterate * actor_count;

      // TODO: should be in array_actor_t
      self->updaters = (array_actor_t*)pony_alloc(sizeof(array_actor_t));
      self->updaters->count = actor_count;
      self->updaters->array = (pony_actor_t**)pony_alloc(
        actor_count * sizeof(pony_actor_t*));

      for(uint64_t i = 0; i < actor_count; i++)
      {
        self->updaters->array[i] = updater_create(
          actor, self->updaters, i, size, chunk, chunk * iterate * i
          );
      }

      gettimeofday(&self->start, NULL);

      for(uint64_t i = 0; i < actor_count; i++)
        updater_apply(self->updaters->array[i], iterate);
      break;
    }

    case MSG_DONE:
    {
      self->done_count++;

      if(self->done_count == self->updaters->count)
      {
        for(uint64_t i = 0; i < self->updaters->count; i++)
          updater_done(self->updaters->array[i]);
      }
      break;
    }

    case MSG_CONF:
    {
      self->conf_count++;

      if(self->conf_count == self->updaters->count)
      {
        struct timeval now;
        gettimeofday(&now, NULL);

        if(self->start.tv_usec > now.tv_usec)
        {
          now.tv_usec += 1000000;
          now.tv_sec--;
        }

        now.tv_sec -= self->start.tv_sec;
        now.tv_usec -= self->start.tv_usec;
        double t = now.tv_sec + (now.tv_usec * 0.000001);

        printf("Time: %g\nGUPS %g\n", t, self->updates / t / 1e9);
      }
      break;
    }
  }
}

int main(int argc, char** argv)
{
  argc = pony_init(argc, argv);
  pony_actor_t* actor = pony_create(&type);
  pony_sendargs(actor, MSG_ARGS, argc, argv);

  return pony_start(PONY_DONT_WAIT);
}

void main_done(pony_actor_t* main)
{
  pony_send(main, MSG_DONE);
}

void main_conf(pony_actor_t* main)
{
  pony_send(main, MSG_CONF);
}
