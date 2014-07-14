#define __STDC_FORMAT_MACROS
#include <pony/pony.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <assert.h>

typedef struct ring_t
{
  pony_actor_t* worker;
  pony_actor_t* next;
  int size;
  int pass;
  int repeat;
} ring_t;

enum
{
  MSG_INIT,
  MSG_NEXT,
  MSG_PASS,
  MSG_WORK
};

static void trace(void* p);
static pony_msg_t* message_type(uint64_t id);
static void dispatch(pony_actor_t* self, void* p, uint64_t id,
  int argc, pony_arg_t* argv);

static pony_actor_type_t type =
{
  2,
  {sizeof(ring_t), trace, NULL, NULL},
  message_type,
  dispatch,
  NULL
};

static pony_msg_t m_main = {2, {PONY_NONE}};
static pony_msg_t m_init = {3, {PONY_NONE}};
static pony_msg_t m_next = {1, {PONY_ACTOR}};
static pony_msg_t m_pass = {1, {PONY_NONE}};
static pony_msg_t m_work = {0, {PONY_NONE}};

static void trace(void* p)
{
  ring_t* d = p;
  pony_traceactor(d->worker);
  pony_traceactor(d->next);
}

static pony_msg_t* message_type(uint64_t id)
{
  switch(id)
  {
    case PONY_MAIN: return &m_main;
    case MSG_INIT: return &m_init;
    case MSG_NEXT: return &m_next;
    case MSG_PASS: return &m_pass;
    case MSG_WORK: return &m_work;
  }

  return NULL;
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
  uint64_t count = factorize(86028157UL * 329545133UL, 2, list);

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
    pony_sendp(actor, MSG_NEXT, next);
    next = actor;
  }

  if(pass > 0) pony_sendi(first, MSG_PASS, pass * size);
  return next;
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
      int size = 50;
      int count = 20;
      int pass = 10000;
      int repeat = 5;

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
        } else if(!strcmp(margv[i], "--count")) {
          if(margc <= (i + 1))
          {
            usage();
            return;
          }

          count = atoi(margv[++i]);
        } else if(!strcmp(margv[i], "--pass")) {
          if(margc <= (i + 1))
          {
            usage();
            return;
          }

          pass = atoi(margv[++i]);
        } else if(!strcmp(margv[i], "--repeat")) {
          if(margc <= (i + 1))
          {
            usage();
            return;
          }

          repeat = atoi(margv[++i]);
        } else {
          usage();
          return;
        }
      }

      argv[0].i = size;
      argv[1].i = pass;
      argv[2].i = repeat;

      for(int i = 0; i < count; i++)
      {
        pony_sendv(pony_create(&type), MSG_INIT, 3, argv);
      }
      break;
    }

    case MSG_INIT:
    {
      d = pony_alloc(sizeof(ring_t));
      pony_set(d);

      d->worker = pony_create(&type);
      d->size = argv[0].i;
      d->pass = argv[1].i;
      d->repeat = argv[2].i;

      pony_send(d->worker, MSG_WORK);
      d->next = spawn_ring(self, d->size, d->pass);
      break;
    }

    case MSG_NEXT:
    {
      d = pony_alloc(sizeof(ring_t));
      pony_set(d);

      d->worker = NULL;
      d->next = argv[0].p;
      d->size = 0;
      d->pass = 0;
      d->repeat = 0;
      break;
    }

    case MSG_PASS:
    {
      if(argv[0].i > 0)
      {
        pony_sendi(d->next, MSG_PASS, argv[0].i - 1);
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
  return pony_start(argc, argv, pony_create(&type));
}
