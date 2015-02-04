#define __STDC_FORMAT_MACROS
#include <pony/pony.h>
#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <assert.h>

typedef struct spreader_msg_t
{
  pony_msg_t msg;
  pony_actor_t* parent;
  uint64_t count;
} spreader_msg_t;

typedef struct spreader_t
{
  pony_actor_pad_t pad;

  pony_actor_t* parent;
  uint64_t count;
  uint64_t result;
  uint64_t received;
} spreader_t;

enum
{
  MSG_ARGS,
  MSG_SPREAD,
  MSG_RESULT
};

static void spawn_child(pony_actor_t* self, spreader_t* d);

static void trace(void* p)
{
  spreader_t* d = (spreader_t*)p;
  pony_traceactor(d->parent);
}

static void dispatch(pony_actor_t* self, pony_msg_t* msg)
{
  spreader_t* d = (spreader_t*)self;

  switch(msg->id)
  {
    case MSG_ARGS:
    {
      pony_main_msg_t* m = (pony_main_msg_t*)msg;

      d->parent = NULL;
      d->result = 0;
      d->received = 0;

      if(m->argc >= 2)
      {
        d->count = atoi(m->argv[1]);
      } else {
        d->count = 10;
      }

      if(d->count > 0)
      {
        spawn_child(self, d);
        spawn_child(self, d);
      } else {
        printf("1 actor\n");
      }
      break;
    }

    case MSG_SPREAD:
    {
      spreader_msg_t* m = (spreader_msg_t*)msg;

      if(m->count == 0)
      {
        pony_sendi(m->parent, MSG_RESULT, 1);
      } else {
        d->parent = m->parent;
        d->count = m->count;
        d->result = 0;
        d->received = 0;

        spawn_child(self, d);
        spawn_child(self, d);
      }
      break;
    }

    case MSG_RESULT:
    {
      pony_msgi_t* m = (pony_msgi_t*)msg;

      assert(d->received < 2);
      d->received++;
      d->result += m->i;

      if(d->received == 2)
      {
        if(d->parent != NULL)
        {
          // sends result + 1 to account for this actor
          pony_sendi(d->parent, MSG_RESULT, d->result + 1);
        } else {
          // will generate 2^(n + 1) - 1 actors, including the first one
          assert((2 << d->count) == (d->result + 2));
          printf("%" PRIu64 " actors\n", d->result + 1);
        }
      }
      break;
    }
  }
}

static pony_type_t type =
{
  2,
  sizeof(spreader_t),
  trace,
  NULL,
  NULL,
  dispatch,
  NULL
};

static void spawn_child(pony_actor_t* self, spreader_t* d)
{
  pony_gc_send();
  pony_traceactor(self);
  pony_send_done();

  spreader_msg_t* m = (spreader_msg_t*)pony_alloc_msg(0, MSG_SPREAD);
  m->parent = self;
  m->count = d->count - 1;

  pony_actor_t* child = pony_create(&type);
  pony_sendv(child, &m->msg);
}

int main(int argc, char** argv)
{
  argc = pony_init(argc, argv);
  pony_actor_t* actor = pony_create(&type);
  pony_sendargs(actor, MSG_ARGS, argc, argv);

  return pony_start(PONY_DONT_WAIT);
}
