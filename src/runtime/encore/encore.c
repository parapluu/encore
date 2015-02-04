#include <pony/pony.h>

/// The starting point of all Encore programs
int encore_start(int argc, char** argv, encore_actor_t *type)
{
  argc = pony_init(argc, argv);
  pony_actor_t* actor = pony_create(type);
  pony_sendargs(actor, MSG_ARGS, argc, argv);

  return pony_start(PONY_DONT_WAIT);
}
