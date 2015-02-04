#include <pony/pony.h>
#include "encore.h"

/// The starting point of all Encore programs
int encore_start(int argc, char** argv, encore_actor_t *type)
{
  argc = pony_init(argc, argv);
  pony_actor_t* actor = pony_create(type);
  pony_sendargs(actor, _ENC__MSG_MAIN, argc, argv);

  return pony_start(PONY_DONT_WAIT);
}
