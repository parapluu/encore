#include <pony/pony.h>
#include <stdlib.h>
#include <stdio.h>

typedef struct counter_t
{
  pony_actor_pad_t pad;
  uint64_t count;
} counter_t;

enum
{
  MSG_ARGS,
  MSG_INIT,
  MSG_INC,
  MSG_GETANDRESET,
  MSG_RESPONSE
};

static void dispatch(pony_actor_t* self, pony_msg_t* msg);

static pony_type_t type =
{
  2,
  sizeof(counter_t),
  NULL,
  NULL,
  NULL,
  dispatch,
  NULL
};

static void dispatch(pony_actor_t* self, pony_msg_t* msg)
{
  counter_t* d = (counter_t*)self;

  switch(msg->id)
  {
    case MSG_ARGS:
    {
      pony_main_msg_t* m = (pony_main_msg_t*)msg;
      uint64_t count = 10;

      if(m->argc >= 2)
        count = atoi(m->argv[1]);

      pony_actor_t* actor = pony_create(&type);
      pony_send(actor, MSG_INIT);

      for(uint64_t i = 0; i < count; i++)
        pony_send(actor, MSG_INC);

      pony_gc_send();
      pony_traceactor(self);
      pony_send_done();

      pony_sendp(actor, MSG_GETANDRESET, self);
      break;
    }

    case MSG_INIT:
      d->count = 0;
      break;

    case MSG_INC:
      d->count++;
      break;

    case MSG_GETANDRESET:
    {
      pony_msgp_t* m = (pony_msgp_t*)msg;
      pony_actor_t* actor = (pony_actor_t*)m->p;

      pony_gc_recv();
      pony_traceactor(actor);
      pony_recv_done();

      pony_sendi(actor, MSG_RESPONSE, d->count);
      d->count = 0;
      break;
    }

    case MSG_RESPONSE:
    {
      pony_msgi_t* m = (pony_msgi_t*)msg;
      printf("%lu\n", m->i);
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
