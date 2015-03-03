#define __STDC_FORMAT_MACROS
#include <pony/pony.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <assert.h>

typedef struct ring_t
{
  pony_actor_pad_t pad;

  pony_actor_t* worker;
  pony_actor_t* next;
  int size;
  int pass;
  int repeat;
} ring_t;

typedef struct ring_msg_t
{
  pony_msg_t msg;
  int size;
  int pass;
  int repeat;
} ring_msg_t;

enum
{
  MSG_ARGS,
  MSG_INIT,
  MSG_NEXT,
  MSG_PASS,
  MSG_WORK
};

static void trace(void* p);
static void dispatch(pony_actor_t* self, pony_msg_t* msg);

static pony_type_t type =
{
  2,
  sizeof(ring_t),
  trace,
  NULL,
  NULL,
  dispatch,
  NULL
};

static void trace(void* p)
{
  ring_t* d = (ring_t*)p;
  pony_traceactor(d->worker);
  pony_traceactor(d->next);
}

static void usage()
{
  printf(
    "mixed OPTIONS\n"
    "  --size N   number of actors in each ring\n"
    "  --count N  number of rings\n"
    "  --pass N   number of messages to pass around each ring\n"
    "  --repeat N number of times to repeat\n"
    );
}

static uint64_t factorize(uint64_t n, uint64_t count, uint64_t* list)
{
  if(n <= 3)
  {
    list[0] = n;
    return 1;
  }

  uint64_t d = 2;
  uint64_t i = 0;

  while(d < n)
  {
    if((n % d) == 0)
    {
      list[i++] = d;
      n /= d;
    } else {
      d = (d == 2) ? 3 : (d + 2);
    }
  }

  list[i++] = d;
  return i;
}

static void test_factorize()
{
  uint64_t list[2];
  uint64_t a = 86028157UL;
  uint64_t b = 329545133UL;

  uint64_t count = factorize(a*b, 2, list);

  if((count != 2) || (list[0] != 86028157) || (list[1] != 329545133))
  {
    printf("factorization error\n");
  } else {
    printf("factorization done\n");
  }
}

static pony_actor_t* spawn_ring(pony_actor_t* first, int size, int pass)
{
  pony_actor_t* next = first;

  for(int i = 0; i < (size - 1); i++)
  {
    pony_actor_t* actor = pony_create(&type);

    pony_gc_send();
    pony_traceactor(next);
    pony_send_done();

    pony_sendp(actor, MSG_NEXT, next);
    next = actor;
  }

  if(pass > 0)
    pony_sendi(first, MSG_PASS, pass * size);

  return next;
}

static void dispatch(pony_actor_t* self, pony_msg_t* msg)
{
  ring_t* d = (ring_t*)self;

  switch(msg->id)
  {
    case MSG_ARGS:
    {
      pony_main_msg_t* m = (pony_main_msg_t*)msg;

      int size = 50;
      int count = 20;
      int pass = 10000;
      int repeat = 5;

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
        } else if(!strcmp(m->argv[i], "--count")) {
          if(m->argc <= (i + 1))
          {
            usage();
            return;
          }

          count = atoi(m->argv[++i]);
        } else if(!strcmp(m->argv[i], "--pass")) {
          if(m->argc <= (i + 1))
          {
            usage();
            return;
          }

          pass = atoi(m->argv[++i]);
        } else if(!strcmp(m->argv[i], "--repeat")) {
          if(m->argc <= (i + 1))
          {
            usage();
            return;
          }

          repeat = atoi(m->argv[++i]);
        } else {
          usage();
          return;
        }
      }

      for(int i = 0; i < count; i++)
      {
        ring_msg_t* m = (ring_msg_t*)pony_alloc_msg(0, MSG_INIT);
        m->size = size;
        m->pass = pass;
        m->repeat = repeat;

        pony_sendv(pony_create(&type), &m->msg);
      }
      break;
    }

    case MSG_INIT:
    {
      ring_msg_t* m = (ring_msg_t*)msg;

      d->worker = pony_create(&type);
      d->size = m->size;
      d->pass = m->pass;
      d->repeat = m->repeat;

      pony_send(d->worker, MSG_WORK);
      d->next = spawn_ring(self, d->size, d->pass);
      break;
    }

    case MSG_NEXT:
    {
      pony_msgp_t* m = (pony_msgp_t*)msg;
      pony_actor_t* actor = (pony_actor_t*)m->p;

      pony_gc_recv();
      pony_traceactor(actor);
      pony_recv_done();

      d->worker = NULL;
      d->next = actor;
      d->size = 0;
      d->pass = 0;
      d->repeat = 0;
      break;
    }

    case MSG_PASS:
    {
      pony_msgi_t* m = (pony_msgi_t*)msg;

      if(m->i > 0)
      {
        pony_sendi(d->next, MSG_PASS, m->i - 1);
      } else {
        assert(d->repeat > 0);
        assert(d->worker != NULL);
        d->repeat--;

        printf("message cycle done\n");

        if(d->repeat > 0)
        {
          pony_send(d->worker, MSG_WORK);
          d->next = spawn_ring(self, d->size, d->pass);
        }
      }
      break;
    }

    case MSG_WORK:
    {
      test_factorize();
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
