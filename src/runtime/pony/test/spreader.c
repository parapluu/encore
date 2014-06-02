#define __STDC_FORMAT_MACROS
#include <pony/pony.h>
#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <assert.h>

typedef struct spreader_t
{
  pony_actor_t* parent;
  uint64_t count;
  uint64_t result;
  uint64_t received;
} spreader_t;

enum
{
  MSG_SPREAD,
  MSG_RESULT
};

static void spawn_children(pony_actor_t* this, spreader_t* d);

static pony_msg_t m_spread = {2, {{NULL, 0, PONY_ACTOR}, {NULL, 8, PONY_PRIMITIVE}}};
static pony_msg_t m_result = {1, {{NULL, 8, PONY_PRIMITIVE}}};

static void trace(void* p)
{
  spreader_t* d = p;
  pony_traceactor(&d->parent);
  pony_trace64(&d->count);
  pony_trace64(&d->result);
  pony_trace64(&d->received);
}

static pony_msg_t* message_type(uint64_t id)
{
  switch(id)
  {
    case MSG_SPREAD: return &m_spread;
    case MSG_RESULT: return &m_result;
  }

  return NULL;
}

static void dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv)
{
  spreader_t* d = p;

  switch(id)
  {
    case PONY_MAIN:
    {
      int margc = argv[0].i;
      char** margv = argv[1].p;

      d = pony_alloc(sizeof(spreader_t));
      pony_set(d);

      d->parent = NULL;
      d->result = 0;
      d->received = 0;

      if(margc >= 2)
      {
        d->count = atoi(margv[1]);
      } else {
        d->count = 19;
      }

      if(d->count > 0)
      {
        spawn_children(this, d);
      } else {
        printf("1 actor\n");
      }
      break;
    }

    case MSG_SPREAD:
    {
      if(argv[1].i == 0)
      {
        pony_sendi(argv[0].p, MSG_RESULT, 1);
      } else {
        d = pony_alloc(sizeof(spreader_t));
        pony_set(d);

        d->parent = argv[0].p;
        d->count = argv[1].i;
        d->result = 0;
        d->received = 0;

        spawn_children(this, d);
      }
      break;
    }

    case MSG_RESULT:
    {
      assert(d->received < 2);
      d->received++;
      d->result += argv[0].i;

      if(d->received == 2)
      {
        if(d->parent != NULL)
        {
          // sends result + 1 to account for this actor
          pony_sendi(d->parent, MSG_RESULT, d->result + 1);
        } else {
          assert((2 << d->count) == (d->result + 2));
          printf("%" PRIu64 " actors\n", d->result + 1);
        }
      }
      break;
    }
  }
}

static pony_actor_type_t type =
{
  1,
  {trace, sizeof(spreader_t), PONY_ACTOR},
  message_type,
  dispatch
};

static void spawn_children(pony_actor_t* this, spreader_t* d)
{
  pony_actor_t* child;
  pony_arg_t arg[2];
  arg[0].p = this;
  arg[1].i = d->count - 1;

  child = pony_create(&type);
  pony_sendv(child, MSG_SPREAD, 2, arg);

  child = pony_create(&type);
  pony_sendv(child, MSG_SPREAD, 2, arg);
}

int main(int argc, char** argv)
{
  return pony_start(argc, argv, pony_create(&type));
}
