#include "main.h"
#include "arrays.h"
#include "streamer.h"
#include "updater.h"

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct main_t
{
  pony_actor_pad_t pad;

  struct timeval total;
  uint64_t streamer_count;
  uint64_t updater_count;
  uint64_t updates;
  array_actor_t* updaters;
} main_t;

typedef struct updater_done_msg_t
{
  pony_msg_t msg;
  struct timeval tv;
} updater_done_msg_t;

enum
{
  MSG_ARGS,
  MSG_STREAMER_DONE,
  MSG_UPDATER_DONE
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
    "  --streamers N  number of streamers (def 4)\n"
    "  --updaters N   number of updaters (def 8)\n"
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

      self->total.tv_sec = 0;
      self->total.tv_usec = 0;
      self->streamer_count = 4;
      self->updater_count = 8;

      uint64_t logtable = 20;
      uint64_t iterate = 10000;
      uint64_t chunk = 1024;

      for(int i = 1; i < m->argc; i++)
      {
        if(getarg_u64("--logtable", &m->argv[i], &logtable) ||
          getarg_u64("--iterate", &m->argv[i], &iterate) ||
          getarg_u64("--chunk", &m->argv[i], &chunk) ||
          getarg_u64("--streamers", &m->argv[i], &self->streamer_count) ||
          getarg_u64("--updaters", &m->argv[i], &self->updater_count)
          )
        {
          i++;
        } else {
          usage();
          return;
        }
      }

      self->updater_count = next_pow2(self->updater_count);
      uint64_t size = ((uint64_t)1 << logtable) / self->updater_count;
      self->updates = chunk * iterate * self->streamer_count;

      //TODO: should be in array_actor_t
      self->updaters = (array_actor_t*)pony_alloc(sizeof(array_actor_t));
      self->updaters->count = self->updater_count;
      self->updaters->array = (pony_actor_t**)pony_alloc(
        self->updaters->count * sizeof(pony_actor_t*));

      for(uint64_t i = 0; i < self->updater_count; i++)
        self->updaters->array[i] = updater_create(i, size);

      for(uint64_t i = 0; i < self->streamer_count; i++)
      {
        streamer_apply(
          streamer_create(
            actor, self->updaters, size, chunk, chunk * iterate * i
            ),
          iterate
          );
      }
      break;
    }

    case MSG_STREAMER_DONE:
    {
      self->streamer_count--;

      if(self->streamer_count == 0)
      {
        for(uint64_t i = 0; i < self->updaters->count; i++)
          updater_done(self->updaters->array[i], actor);
      }
      break;
    }

    case MSG_UPDATER_DONE:
    {
      updater_done_msg_t* m = (updater_done_msg_t*)msg;

      self->total.tv_sec += m->tv.tv_usec;
      self->total.tv_usec += m->tv.tv_usec;

      if(self->total.tv_usec >= 1000000)
      {
        self->total.tv_sec++;
        self->total.tv_usec -= 1000000;
      }

      self->updater_count--;

      if(self->updater_count == 0)
      {
        double t = self->total.tv_sec + (self->total.tv_usec * 0.000001);
        t /= self->updaters->count;
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

void main_streamerdone(pony_actor_t* main)
{
  pony_send(main, MSG_STREAMER_DONE);
}

void main_updaterdone(pony_actor_t* main, struct timeval* tv)
{
  updater_done_msg_t* m = (updater_done_msg_t*)pony_alloc_msg(
    0, MSG_UPDATER_DONE);

  m->tv = *tv;
  pony_sendv(main, &m->msg);
}
