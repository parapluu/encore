#include "updater.h"
#include "main.h"

typedef struct updater_t
{
  struct timeval start;
  uint64_t count;
  uint64_t* table;
} updater_t;

enum
{
  MSG_CREATE,
  MSG_APPLY,
  MSG_DONE,
};

static void trace_updater(void* p);
static pony_msg_t* msg_updater(uint64_t id);
static void do_updater(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv);

static pony_actor_type_t type =
{
  3,
  {trace_updater, sizeof(updater_t), PONY_ACTOR},
  msg_updater,
  do_updater
};

static pony_msg_t m_create = {2, {{NULL, 8, PONY_PRIMITIVE}, {NULL, 8, PONY_PRIMITIVE}}};
static pony_msg_t m_apply = {2, {{NULL, 8, PONY_PRIMITIVE}, {NULL, 0, PONY_IMMUTABLE}}};
static pony_msg_t m_done = {1, {{NULL, 0, PONY_ACTOR}}};

static void trace_updater(void* p)
{
  updater_t* this = p;
  // pony_trace64(&this->start.tv_sec);
  // pony_trace64(&this->start.tv_usec);
  // pony_trace64(&this->count);
  pony_trace(&this->table, NULL, this->count * sizeof(uint64_t), PONY_MUTABLE);
}

static pony_msg_t* msg_updater(uint64_t id)
{
  switch(id)
  {
    case MSG_CREATE: return &m_create;
    case MSG_APPLY: return &m_apply;
    case MSG_DONE: return &m_done;
  }

  return NULL;
}

static void do_updater(pony_actor_t* actor, void* p, uint64_t id, int argc, pony_arg_t* argv)
{
  updater_t* this = p;

  switch(id)
  {
    case MSG_CREATE:
    {
      this = pony_alloc(sizeof(updater_t));
      pony_set(this);

      uint64_t index = argv[0].i;
      uint64_t size = argv[1].i;
      uint64_t offset = index * size;

      this->count = size;
      this->table = pony_alloc(this->count * sizeof(uint64_t));

      for(uint64_t i = 0; i < size; i++)
        this->table[i] = i + offset;

      gettimeofday(&this->start, NULL);
      break;
    }

    case MSG_APPLY:
    {
      const uint64_t size = argv[0].i;
      const uint64_t* data = argv[1].p;
      const uint64_t mask = this->count - 1;
      uint64_t* table = this->table;

#if 0
      for(uint32_t i = 0; i < size; i++)
      {
        const uint64_t datum1 = data[i];
        const uint64_t index1 = datum1 & mask;
        table[index1] ^= datum1;
      }
#else
      for(uint32_t i = 0; i < size; i += 8)
      {
        const uint64_t datum1 = data[i];
        const uint64_t datum2 = data[i + 1];
        const uint64_t datum3 = data[i + 2];
        const uint64_t datum4 = data[i + 3];
        const uint64_t datum5 = data[i + 4];
        const uint64_t datum6 = data[i + 5];
        const uint64_t datum7 = data[i + 6];
        const uint64_t datum8 = data[i + 7];

        const uint64_t index1 = datum1 & mask;
        const uint64_t index2 = datum2 & mask;
        const uint64_t index3 = datum3 & mask;
        const uint64_t index4 = datum4 & mask;
        const uint64_t index5 = datum5 & mask;
        const uint64_t index6 = datum6 & mask;
        const uint64_t index7 = datum7 & mask;
        const uint64_t index8 = datum8 & mask;

        table[index1] ^= datum1;
        table[index2] ^= datum2;
        table[index3] ^= datum3;
        table[index4] ^= datum4;
        table[index5] ^= datum5;
        table[index6] ^= datum6;
        table[index7] ^= datum7;
        table[index8] ^= datum8;
      }
#endif
      // FIX: provide a 'free' function for the array
      // doesn't actually free of course
      // if the memory belongs to another actor, send an RCDEC immediately
      // if it belongs to us and foreign rc = 0, free it
      // otherwise, indicate it can be freed as soon as the foreign rc
      // drops to 0? problematic: we could receive it back again in a message
      // would have to check everything we receive that belongs to us and
      // clear this mark

      // this is only needed because received messages aren't counted towards
      // the size of our working set.
      pony_triggergc();
      break;
    }

    case MSG_DONE:
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

      main_updaterdone(argv[0].p, &now);
      break;
    }
  }
}

pony_actor_t* updater_create(uint64_t index, uint64_t size)
{
  pony_actor_t* updater = pony_create(&type);

  pony_arg_t argv[2];
  argv[0].i = index;
  argv[1].i = size;
  pony_sendv(updater, MSG_CREATE, 2, argv);

  return updater;
}

void updater_apply(pony_actor_t* updater, uint64_t size, uint64_t* data)
{
  pony_arg_t arg[2];
  arg[0].i = size;
  arg[1].p = data;
  pony_sendv(updater, MSG_APPLY, 2, arg);
}

void updater_done(pony_actor_t* updater, pony_actor_t* main)
{
  pony_sendp(updater, MSG_DONE, main);
}
