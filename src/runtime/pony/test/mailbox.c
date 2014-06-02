#define __STDC_FORMAT_MACROS
#include <pony/pony.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <assert.h>

enum
{
  MSG_MAILBOX,
  MSG_PING
};

static pony_msg_t* message_type(uint64_t id);
static void dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv);

static pony_actor_type_t type =
{
  1,
  {NULL, 0, PONY_ACTOR},
  message_type,
  dispatch
};

static pony_msg_t m_mailbox = {2, {{NULL, 0, PONY_ACTOR}, {NULL, 64, PONY_PRIMITIVE}}};
static pony_msg_t m_ping = {0, {{NULL, 0, PONY_PRIMITIVE}}};

static pony_msg_t* message_type(uint64_t id)
{
  switch(id)
  {
    case MSG_MAILBOX: return &m_mailbox;
    case MSG_PING: return &m_ping;
  }

  return NULL;
}

static void usage()
{
  printf(
    "mailbox OPTIONS\n"
    "  --size N   number of sending actors\n"
    "  --pass N   number of messages to pass from each sender to the receiver\n"
    );
}

static void dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv)
{
  switch(id)
  {
    case PONY_MAIN:
    {
      int margc = argv[0].i;
      char** margv = argv[1].p;
      int size = 3;
      int pass = 0;

      for(int i = 1; i < margc; i++)
      {
        if(!strcmp(margv[i], "--size"))
        {
          if(margc <= (i + 1))
          {
            usage();
            return;
          }

          size = atoi(margv[++i]);
        } else if(!strcmp(margv[i], "--pass")) {
          if(margc <= (i + 1))
          {
            usage();
            return;
          }

          pass = atoi(margv[++i]);
        } else {
          usage();
          return;
        }
      }

      pony_actor_t* actor;
      pony_arg_t argv[2];
      argv[0].p = this;
      argv[1].i = pass;

      for(int i = 0; i < size; i++)
      {
        actor = pony_create(&type);
        pony_sendv(actor, MSG_MAILBOX, 2, argv);
      }
      break;
    }

    case MSG_MAILBOX:
    {
      pony_actor_t* receiver = argv[0].p;
      int pass = argv[1].i;

      for(int i = 0; i < pass; i++)
      {
        pony_send(receiver, MSG_PING);
      }
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
  return pony_start(argc, argv, pony_create(&type));
}
