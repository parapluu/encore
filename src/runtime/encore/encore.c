#include <pony/pony.h>

typedef struct encore_actor encore_actor_t;

struct encore_actor {
  pony_actor_pad_t pad;
  // Everything else that goes into an encore_actor that's not part of PonyRT
};

/// The starting point of all Encore programs
int encore_start(int argc, char** argv, encore_actor_t *type)
{
  argc = pony_init(argc, argv);
  pony_actor_t* actor = pony_create(type);
  pony_sendargs(actor, MSG_ARGS, argc, argv);

  return pony_start(PONY_DONT_WAIT);
}
