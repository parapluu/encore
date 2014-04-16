#include <pony/pony.h>
#include <stdlib.h>
#include <stdio.h>

/* Hello Ponyworld! */

typedef struct main_t
{
} main_t;


static void trace(void* p)
{
}

static pony_msg_t* message_type(uint64_t id)
{
  return NULL;
}

static void dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv);

static pony_actor_type_t type =
{
  1,
  {trace, sizeof(main_t), PONY_ACTOR},
  message_type,
  dispatch
};

static void dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv)
{
  main_t* d = p;

  switch(id)
  {
    case PONY_MAIN:
    {
	    printf("Hello Ponyworld!\n")
    }
}

int main(int argc, char** argv)
{
  return pony_start(argc, argv, pony_create(&type));
}