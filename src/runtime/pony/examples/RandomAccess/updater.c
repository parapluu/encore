#include "updater.h"
#include "main.h"

typedef struct updater_t
{
  pony_actor_pad_t pad;
  struct timeval start;
  uint64_t count;
  uint64_t* table;
} updater_t;

typedef struct create_msg_t
{
  pony_msg_t msg;
  uint64_t index;
  uint64_t size;
} create_msg_t;

typedef struct apply_msg_t
{
  pony_msg_t msg;
  uint64_t size;
  uint64_t* data;
} apply_msg_t;

enum
{
  MSG_CREATE,
  MSG_APPLY,
  MSG_DONE,
};

static void trace_updater(void* p);
static void do_updater(pony_actor_t* self, pony_msg_t* msg);

static pony_type_t type =
{
  3,
  sizeof(updater_t),
  trace_updater,
  NULL,
  NULL,
  do_updater,
  NULL
};

static void trace_updater(void* p)
{
  updater_t* self = (updater_t*)p;
  pony_trace(self->table);
}

static void do_updater(pony_actor_t* actor, pony_msg_t* msg)
{
  updater_t* self = (updater_t*)actor;

  switch(msg->id)
  {
    case MSG_CREATE:
    {
      create_msg_t* m = (create_msg_t*)msg;

      uint64_t index = m->index;
      uint64_t size = m->size;
      uint64_t offset = index * size;

      self->count = size;
      self->table = (uint64_t*)pony_alloc(self->count * sizeof(uint64_t));

      for(uint64_t i = 0; i < size; i++)
        self->table[i] = i + offset;

      gettimeofday(&self->start, NULL);
      break;
    }

    case MSG_APPLY:
    {
      apply_msg_t* m = (apply_msg_t*)msg;

      pony_gc_recv();
      pony_trace(m->data);
      pony_recv_done();

      const uint64_t size = m->size;
      const uint64_t* data = m->data;
      const uint64_t mask = self->count - 1;
      uint64_t* table = self->table;

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
      break;
    }

    case MSG_DONE:
    {
      pony_msgp_t* m = (pony_msgp_t*)msg;
      pony_actor_t* main_actor = (pony_actor_t*)m->p;

      pony_gc_recv();
      pony_traceactor(main_actor);
      pony_recv_done();

      struct timeval now;
      gettimeofday(&now, NULL);

      if(self->start.tv_usec > now.tv_usec)
      {
        now.tv_usec += 1000000;
        now.tv_sec--;
      }

      now.tv_sec -= self->start.tv_sec;
      now.tv_usec -= self->start.tv_usec;

      main_updaterdone(main_actor, &now);
      break;
    }
  }
}

pony_actor_t* updater_create(uint64_t index, uint64_t size)
{
  pony_actor_t* updater = pony_create(&type);

  create_msg_t* m = (create_msg_t*)pony_alloc_msg(0, MSG_CREATE);
  m->index = index;
  m->size = size;

  pony_sendv(updater, &m->msg);
  return updater;
}

void updater_apply(pony_actor_t* updater, uint64_t size, uint64_t* data)
{
  pony_gc_send();
  pony_trace(data);
  pony_send_done();

  apply_msg_t* m = (apply_msg_t*)pony_alloc_msg(0, MSG_APPLY);
  m->size = size;
  m->data = data;

  pony_sendv(updater, &m->msg);
}

void updater_done(pony_actor_t* updater, pony_actor_t* main)
{
  pony_gc_send();
  pony_traceactor(main);
  pony_send_done();

  pony_sendp(updater, MSG_DONE, main);
}
