#include "main.h"
#include "arrays.h"
#include "streamer.h"
#include "updater.h"

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct ring_t
{
  struct timeval total;
  uint64_t streamer_count;
  uint64_t updater_count;
  uint64_t updates;
  array_actor_t* updaters;
} main_t;

enum
{
  MSG_STREAMER_DONE,
  MSG_UPDATER_DONE
};

static void trace(void* p);
static pony_msg_t* message_type(uint64_t id);
static void dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv);

static pony_actor_type_t type =
{
  1,
  {trace, sizeof(main_t), PONY_ACTOR},
  message_type,
  dispatch
};

static pony_msg_t m_streamer_done = {0, {{NULL, 0, PONY_PRIMITIVE}}};
static pony_msg_t m_updater_done = {2, {{NULL, 8, PONY_PRIMITIVE}}};

static void trace(void* p)
{
  main_t* this = p;
  // pony_trace64(&this->streamer_count);
  // pony_trace64(&this->updater_count);
  // pony_trace64(&this->updates);
  pony_trace(&this->updaters, array_actor_trace, sizeof(array_actor_t), PONY_IMMUTABLE);
}

static pony_msg_t* message_type(uint64_t id)
{
  switch(id)
  {
    case MSG_STREAMER_DONE: return &m_streamer_done;
    case MSG_UPDATER_DONE: return &m_updater_done;
  }

  return NULL;
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

static void dispatch(pony_actor_t* actor, void* p, uint64_t id, int argc, pony_arg_t* argv)
{
  main_t* this = p;

  switch(id)
  {
    case PONY_MAIN:
    {
      int margc = argv[0].i;
      char** margv = argv[1].p;

      this = pony_alloc(sizeof(main_t));
      pony_set(this);

      this->total.tv_sec = 0;
      this->total.tv_usec = 0;
      this->streamer_count = 4;
      this->updater_count = 8;

      uint64_t logtable = 20;
      uint64_t iterate = 10000;
      uint64_t chunk = 1024;

      for(int i = 1; i < margc; i++)
      {
        if(getarg_u64("--logtable", &margv[i], &logtable) ||
          getarg_u64("--iterate", &margv[i], &iterate) ||
          getarg_u64("--chunk", &margv[i], &chunk) ||
          getarg_u64("--streamers", &margv[i], &this->streamer_count) ||
          getarg_u64("--updaters", &margv[i], &this->updater_count)
          )
        {
          i++;
        } else {
          usage();
          return;
        }
      }

      this->updater_count = next_pow2(this->updater_count);
      uint64_t size = (1 << logtable) / this->updater_count;
      this->updates = chunk * iterate * this->streamer_count;

      // FIX: should be in array_actor_t
      this->updaters = pony_alloc(sizeof(array_actor_t));
      this->updaters->count = this->updater_count;
      this->updaters->array = pony_alloc(this->updaters->count * sizeof(pony_actor_t*));

      for(uint64_t i = 0; i < this->updater_count; i++)
        this->updaters->array[i] = updater_create(i, size);

      for(uint64_t i = 0; i < this->streamer_count; i++)
      {
        streamer_apply(
          streamer_create(
            actor, this->updaters, size, chunk, chunk * iterate * i
            ),
          iterate
          );
      }
      break;
    }

    case MSG_STREAMER_DONE:
    {
      this->streamer_count--;

      if(this->streamer_count == 0)
      {
        for(uint64_t i = 0; i < this->updaters->count; i++)
          updater_done(this->updaters->array[i], actor);
      }
      break;
    }

    case MSG_UPDATER_DONE:
    {
      this->total.tv_sec += argv[0].i;
      this->total.tv_usec += argv[1].i;

      if(this->total.tv_usec >= 1000000)
      {
        this->total.tv_sec++;
        this->total.tv_usec -= 1000000;
      }

      this->updater_count--;

      if(this->updater_count == 0)
      {
        double t = this->total.tv_sec + (this->total.tv_usec * 0.000001);
        t /= this->updaters->count;
        printf("Time: %g\nGUPS %g\n", t, this->updates / t / 1e9);
      }
      break;
    }
  }
}

int main(int argc, char** argv)
{
  return pony_start(argc, argv, pony_create(&type));
}

void main_streamerdone(pony_actor_t* main)
{
  pony_send(main, MSG_STREAMER_DONE);
}

void main_updaterdone(pony_actor_t* main, struct timeval* tv)
{
  pony_arg_t arg[2];
  arg[0].i = tv->tv_sec;
  arg[1].i = tv->tv_usec;
  pony_sendv(main, MSG_UPDATER_DONE, 2, arg);
}
