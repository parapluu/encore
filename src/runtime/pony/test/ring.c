#define __STDC_FORMAT_MACROS
#include <pony/pony.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <assert.h>

typedef struct ring_t
{
  pony_actor_t* next;
} ring_t;

enum
{
  MSG_INIT,
  MSG_PASS,
};

static void trace(void* p);
static pony_msg_t* message_type(uint64_t id);
static void dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv);

static pony_actor_type_t type =
{
  1,
  {trace, sizeof(ring_t), PONY_ACTOR},
  message_type,
  dispatch
};

static pony_msg_t m_init = {1, {{NULL, 0, PONY_ACTOR}}};
static pony_msg_t m_pass = {1, {{NULL, 8, PONY_PRIMITIVE}}};

static void trace(void* p)
{
  ring_t* d = p;
  pony_traceactor(&d->next);
}

static pony_msg_t* message_type(uint64_t id)
{
  switch(id)
  {
    case MSG_INIT: return &m_init;
    case MSG_PASS: return &m_pass;
  }

  return NULL;
}

static void usage()
{
  printf(
    "ring OPTIONS\n"
    "  --size N   number of actors in each ring\n"
    "  --count N  number of rings\n"
    "  --pass N   number of messages to pass around each ring\n"
    );
}

static void dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv)
{
  ring_t* d = p;

  switch(id)
  {
    case PONY_MAIN:
    {
      int margc = argv[0].i;
      char** margv = argv[1].p;
      int ring_size = 3;
      int ring_count = 1;
      uint64_t pass = 0;

      for(int i = 1; i < margc; i++)
      {
        if(!strcmp(margv[i], "--size"))
        {
          if(margc <= (i + 1))
          {
            usage();
            return;
          }

          ring_size = atoi(margv[++i]);
        } else if(!strcmp(margv[i], "--count")) {
          if(margc <= (i + 1))
          {
            usage();
            return;
          }

          ring_count = atoi(margv[++i]);
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

      for(int i = 0; i < ring_count; i++)
      {
        pony_actor_t* first = pony_create(&type);
        pony_actor_t* next = first;

        for(int j = 0; j < (ring_size - 1); j++)
        {
          pony_actor_t* actor = pony_create(&type);
          pony_sendp(actor, MSG_INIT, next);
          next = actor;
        }

        pony_sendp(first, MSG_INIT, next);

        if(pass > 0) pony_sendi(first, MSG_PASS, pass);
      }
      break;
    }

    case MSG_INIT:
    {
      d = pony_alloc(sizeof(ring_t));
      d->next = argv[0].p;
      pony_set(d);
      break;
    }

    case MSG_PASS:
    {
      if(argv[0].i > 0)
      {
        pony_sendi(d->next, MSG_PASS, argv[0].i - 1);
      }
      break;
    }
  }
}

int main(int argc, char** argv)
{
  return pony_start(argc, argv, pony_create(&type));
}
