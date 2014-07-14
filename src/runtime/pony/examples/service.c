#define __STDC_FORMAT_MACROS
#include <pony/pony.h>
#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <assert.h>

typedef struct service_t
{
  uint64_t expect;
  uint64_t received;
} service_t;

enum
{
  MSG_PING,
  MSG_PONG
};

static pony_msg_t m_ping = {1, {PONY_ACTOR}};
static pony_msg_t m_pong = {0, {PONY_NONE}};

static pony_msg_t* message_type(uint64_t id)
{
  switch(id)
  {
    case MSG_PING: return &m_ping;
    case MSG_PONG: return &m_pong;
  }

  return NULL;
}

static void dispatch(pony_actor_t* self, void* p, uint64_t id,
  int argc, pony_arg_t* argv)
{
  service_t* d = p;

  switch(id)
  {
    case MSG_PING:
    {
      assert(d == NULL);
      pony_send(argv[0].p, MSG_PONG);
      break;
    }

    case MSG_PONG:
    {
      assert(d != NULL);
      d->received++;
      break;
    }
  }
}

static pony_actor_type_t type =
{
  1,
  {sizeof(service_t), NULL, NULL, NULL},
  message_type,
  dispatch,
  NULL
};

static void waitfor(int count)
{
  pony_actor_t* actor = pony_create(&type);
  pony_become(actor);
  pony_unschedule();

  service_t* d = pony_alloc(sizeof(service_t));
  d->expect = count;
  d->received = 0;
  pony_set(d);

  for(int i = 0; i < count; i++)
  {
    pony_actor_t* ping = pony_create(&type);
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

  int count = 10;

  if(argc >= 2)
    count = atoi(argv[1]);

  for(int i = 1; i <= count; i++)
    waitfor(i);

  return pony_shutdown();
}
