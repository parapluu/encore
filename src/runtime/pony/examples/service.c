#define __STDC_FORMAT_MACROS
#include <pony/pony.h>
#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <assert.h>

typedef struct service_t
{
  pony_actor_pad_t pad;
  uint64_t expect;
  uint64_t received;
} service_t;

enum
{
  MSG_PING,
  MSG_PONG
};

static void dispatch(pony_actor_t* self, pony_msg_t* msg)
{
  service_t* d = (service_t*)self;

  switch(msg->id)
  {
    case MSG_PING:
    {
      pony_msgp_t* m = (pony_msgp_t*)msg;
      pony_send((pony_actor_t*)m->p, MSG_PONG);
      break;
    }

    case MSG_PONG:
    {
      d->received++;
      break;
    }
  }
}

static pony_type_t type =
{
  2,
  sizeof(service_t),
  NULL,
  NULL,
  NULL,
  dispatch,
  NULL
};

static void waitfor(int count)
{
  pony_actor_t* actor = pony_create(&type);
  pony_become(actor);
  pony_unschedule();

  service_t* d = (service_t*)actor;
  d->expect = count;
  d->received = 0;

  for(int i = 0; i < count; i++)
  {
    pony_actor_t* ping = pony_create(&type);

    pony_gc_send();
    pony_traceactor(actor);
    pony_send_done();

    pony_sendp(ping, MSG_PING, actor);
  }

  while(d->received < count)
    pony_poll();

  pony_become(NULL);
  pony_schedule(actor);

  printf("got %d\n", count);
}

int main(int argc, char** argv)
{
  argc = pony_init(argc, argv);
  pony_start(PONY_ASYNC_WAIT);

  int count = 10;

  if(argc >= 2)
    count = atoi(argv[1]);

  for(int i = 1; i <= count; i++)
    waitfor(i);

  return pony_stop();
}
