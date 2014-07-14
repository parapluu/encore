#define __STDC_FORMAT_MACROS
#include <pony/pony.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <assert.h>

static void usage()
{
  printf(
    "ring OPTIONS\n"
    "  --size N   number of actors in each ring\n"
    "  --count N  number of rings\n"
    "  --pass N   number of messages to pass around each ring\n"
    );
}

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
static void dispatch(pony_actor_t* self, void* p, uint64_t id,
  int argc, pony_arg_t* argv);
static void final(void* p);

static pony_actor_type_t type =
{
  1,
  {sizeof(ring_t), trace, NULL, NULL},
  message_type,
  dispatch,
  final
};

static pony_msg_t m_main = {2, {PONY_NONE}};
static pony_msg_t m_init = {1, {PONY_ACTOR}};
static pony_msg_t m_pass = {1, {PONY_NONE}};

static void trace(void* p)
{
  ring_t* d = p;
  pony_traceactor(d->next);
}

static pony_msg_t* message_type(uint64_t id)
{
  switch(id)
  {
    case PONY_MAIN: return &m_main;
    case MSG_INIT: return &m_init;
    case MSG_PASS: return &m_pass;
  }

  return NULL;
}

static void dispatch(pony_actor_t* self, void* p, uint64_t id,
  int argc, pony_arg_t* argv)
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
      uint64_t pass = 10;

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

        if(pass > 0)
          pony_sendi(first, MSG_PASS, pass);
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
        // printf("%zd\n", argv[0].i);
        pony_sendi(d->next, MSG_PASS, argv[0].i - 1);
      } else {
        printf("done\n");
      }
      break;
    }
  }
}

static void final(void* p)
{
  // printf("bye\n");
}

int main(int argc, char** argv)
{
  return pony_start(argc, argv, pony_create(&type));
}
