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
  pony_actor_pad_t pad;
  pony_actor_t* next;
} ring_t;

enum
{
  MSG_ARGS,
  MSG_INIT,
  MSG_PASS
};

static void trace(void* p);
static void dispatch(pony_actor_t* self, pony_msg_t* msg);
static void final(void* p);

static pony_type_t type =
{
  2,
  sizeof(ring_t),
  trace,
  NULL,
  NULL,
  dispatch,
  final
};

static void trace(void* p)
{
  ring_t* d = (ring_t*)p;
  pony_traceactor(d->next);
}

static void dispatch(pony_actor_t* self, pony_msg_t* msg)
{
  ring_t* d = (ring_t*)self;

  switch(msg->id)
  {
    case MSG_ARGS:
    {
      pony_main_msg_t* m = (pony_main_msg_t*)msg;
      int ring_size = 3;
      int ring_count = 1;
      uint64_t pass = 10;

      for(int i = 1; i < m->argc; i++)
      {
        if(!strcmp(m->argv[i], "--size"))
        {
          if(m->argc <= (i + 1))
          {
            usage();
            return;
          }

          ring_size = atoi(m->argv[++i]);
        } else if(!strcmp(m->argv[i], "--count")) {
          if(m->argc <= (i + 1))
          {
            usage();
            return;
          }

          ring_count = atoi(m->argv[++i]);
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

      for(int i = 0; i < ring_count; i++)
      {
        pony_actor_t* first = pony_create(&type);
        pony_actor_t* next = first;

        for(int j = 0; j < (ring_size - 1); j++)
        {
          pony_actor_t* actor = pony_create(&type);

          pony_gc_send();
          pony_traceactor(next);
          pony_send_done();

          pony_sendp(actor, MSG_INIT, next);
          next = actor;
        }

        pony_gc_send();
        pony_traceactor(next);
        pony_send_done();

        pony_sendp(first, MSG_INIT, next);

        if(pass > 0)
          pony_sendi(first, MSG_PASS, pass);
      }
      break;
    }

    case MSG_INIT:
    {
      pony_msgp_t* m = (pony_msgp_t*)msg;
      d->next = (pony_actor_t*)m->p;
      break;
    }

    case MSG_PASS:
    {
      pony_msgi_t* m = (pony_msgi_t*)msg;

      if(m->i > 0)
      {
        // printf("%zd\n", argv[0].i);
        pony_sendi(d->next, MSG_PASS, m->i - 1);
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
  argc = pony_init(argc, argv);
  pony_actor_t* actor = pony_create(&type);
  pony_sendargs(actor, MSG_ARGS, argc, argv);

  return pony_start(PONY_DONT_WAIT);
}
