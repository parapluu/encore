#define __STDC_FORMAT_MACROS
#include <pony/pony.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <assert.h>

typedef struct mailbox_msg_t
{
  pony_msg_t msg;
  pony_actor_t* receiver;
  int pass;
} mailbox_msg_t;

enum
{
  MSG_ARGS,
  MSG_MAILBOX,
  MSG_PING
};

static void dispatch(pony_actor_t* self, pony_msg_t* msg);

static pony_type_t type =
{
  2,
  sizeof(pony_actor_pad_t),
  NULL,
  NULL,
  NULL,
  dispatch,
  NULL
};

static void usage()
{
  printf(
    "mailbox OPTIONS\n"
    "  --size N   number of sending actors\n"
    "  --pass N   number of messages to pass from each sender to the receiver\n"
    );
}

static void dispatch(pony_actor_t* self, pony_msg_t* msg)
{
  switch(msg->id)
  {
    case MSG_ARGS:
    {
      pony_main_msg_t* m = (pony_main_msg_t*)msg;
      int size = 3;
      int pass = 0;

      for(int i = 1; i < m->argc; i++)
      {
        if(!strcmp(m->argv[i], "--size"))
        {
          if(m->argc <= (i + 1))
          {
            usage();
            return;
          }

          size = atoi(m->argv[++i]);
        } else if(!strcmp(m->argv[i], "--pass")) {
          if(m->argc <= (i + 1))
          {
            usage();
            return;
          }

          pass = atoi(m->argv[++i]);
        } else {
          usage();
          return;
        }
      }

      for(int i = 0; i < size; i++)
      {
        pony_actor_t* actor = pony_create(&type);
        mailbox_msg_t* m = (mailbox_msg_t*)pony_alloc_msg(0, MSG_MAILBOX);

        pony_gc_send();
        pony_traceactor(self);
        pony_send_done();

        m->receiver = self;
        m->pass = pass;
        pony_sendv(actor, &m->msg);
      }
      break;
    }

    case MSG_MAILBOX:
    {
      mailbox_msg_t* m = (mailbox_msg_t*)msg;

      for(int i = 0; i < m->pass; i++)
        pony_send(m->receiver, MSG_PING);
      break;
    }

    case MSG_PING:
    {
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
