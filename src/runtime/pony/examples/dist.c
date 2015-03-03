#define __STDC_FORMAT_MACROS
#include <pony/pony.h>

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

enum
{
  MSG_ARGS,
  MSG_INIT
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
    "dist OPTIONS\n"
    "  --actors N number of actors to spawn\n"
  );
}

static void dispatch(pony_actor_t* self, pony_msg_t* msg)
{
  switch(msg->id)
  {
    case MSG_ARGS:
    {
      pony_main_msg_t* m = (pony_main_msg_t*)msg;
      int32_t actors = 100000;

      for(int32_t i = 1; i < m->argc; i++)
      {
        if(!strcmp(m->argv[i], "--actors"))
        {
          if(m->argc <= (i + 1))
          {
            usage();
            return;
          }

          actors = atoi(m->argv[++i]);
        }
      }

      for(int32_t i = 0; i < actors; i++)
      {
        pony_send(pony_create(&type), MSG_INIT);
      }

      break;
    }

    case MSG_INIT:
      //TODO
      break;
  }
}

int main(int argc, char** argv)
{
  argc = pony_init(argc, argv);
  pony_actor_t* actor = pony_create(&type);
  pony_sendargs(actor, MSG_ARGS, argc, argv);

  return pony_start(PONY_DONT_WAIT);
}
