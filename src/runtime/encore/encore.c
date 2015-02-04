#include <pony/pony.h>
#include <string.h>
#include "encore.h"

encore_actor_t *encore_create(pony_type_t *type)
{
  return (encore_actor_t *)pony_create(type);
}

/// Allocate s bytes of memory, zeroed out
void *encore_alloc(size_t s)
{
  void *mem = pony_alloc(s);
  memset(mem, 0, s);

  return mem;
}

/// The starting point of all Encore programs
int encore_start(int argc, char** argv, pony_type_t *type)
{
  argc = pony_init(argc, argv);
  pony_actor_t* actor = (pony_actor_t *)encore_create(type);
  pony_sendargs(actor, _ENC__MSG_MAIN, argc, argv);

  return pony_start(PONY_DONT_WAIT);
}

