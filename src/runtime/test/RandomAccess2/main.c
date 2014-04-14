#include <pony/pony.h>
#include <stdint.h>
#include <stdbool.h>

#include "main.h"
#include "arrays.h"
#include "updater.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>

typedef struct ring_t
{
  uint64_t done_count;
  uint64_t conf_count;
  uint64_t updates;
  array_actor_t* updaters;
  struct timeval start;
} main_t;

enum
{
  MSG_DONE,
  MSG_CONF
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

static pony_msg_t m_done = {0, {{NULL, 0, PONY_PRIMITIVE}}};
static pony_msg_t m_conf = {0, {{NULL, 0, PONY_PRIMITIVE}}};

static void trace(void* p)
{
  main_t* this = p;
  // pony_trace64(&this->done_count);
  // pony_trace64(&this->conf_count);
  // pony_trace64(&this->updates);
  pony_trace(&this->updaters, array_actor_trace, sizeof(array_actor_t), PONY_IMMUTABLE);
  // pony_trace64(&this->start.tv_sec);
  // pony_trace64(&this->start.tv_usec);
}

static pony_msg_t* message_type(uint64_t id)
{
  switch(id)
  {
    case MSG_DONE: return &m_done;
    case MSG_CONF: return &m_conf;
  }

  return NULL;
}

static void usage()
{
  printf(
    "gups OPTIONS\n"
    "  --logtable N   log2 of the total table size (def 21)\n"
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

      this->done_count = 0;
      this->conf_count = 0;

      uint64_t logtable = 20;
      uint64_t iterate = 10000;
      uint64_t chunk = 1024;
      uint64_t actor_count = 4;

      for(int i = 1; i < margc; i++)
      {
        if(getarg_u64("--logtable", &margv[i], &logtable) ||
          getarg_u64("--iterate", &margv[i], &iterate) ||
          getarg_u64("--chunk", &margv[i], &chunk) ||
          getarg_u64("--actors", &margv[i], &actor_count)
          )
        {
          i++;
        } else {
          usage();
          return;
        }
      }

      actor_count = next_pow2(actor_count);
      uint64_t size = (1 << logtable) / actor_count;
      this->updates = chunk * iterate * actor_count;

      // FIX: should be in array_actor_t
      this->updaters = pony_alloc(sizeof(array_actor_t));
      this->updaters->count = actor_count;
      this->updaters->array = pony_alloc(actor_count * sizeof(pony_actor_t*));

      for(uint64_t i = 0; i < actor_count; i++)
      {
        this->updaters->array[i] = updater_create(
          actor, this->updaters, i, size, chunk, chunk * iterate * i
          );
      }

      gettimeofday(&this->start, NULL);

      for(uint64_t i = 0; i < actor_count; i++)
        updater_apply(this->updaters->array[i], iterate);
      break;
    }

    case MSG_DONE:
    {
      this->done_count++;

      if(this->done_count == this->updaters->count)
      {
        for(uint64_t i = 0; i < this->updaters->count; i++)
          updater_done(this->updaters->array[i]);
      }
      break;
    }

    case MSG_CONF:
    {
      this->conf_count++;

      if(this->conf_count == this->updaters->count)
      {
        struct timeval now;
        gettimeofday(&now, NULL);

        if(this->start.tv_usec > now.tv_usec)
        {
          now.tv_usec += 1000000;
          now.tv_sec--;
        }

        now.tv_sec -= this->start.tv_sec;
        now.tv_usec -= this->start.tv_usec;
        double t = now.tv_sec + (now.tv_usec * 0.000001);

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

void main_done(pony_actor_t* main)
{
  pony_send(main, MSG_DONE);
}

void main_conf(pony_actor_t* main)
{
  pony_send(main, MSG_CONF);
}
