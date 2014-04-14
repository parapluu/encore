#ifndef cycle_h
#define cycle_h

#include <pony/pony.h>
#include "map.h"

void cycle_create();

void cycle_block(pony_actor_t* actor, uint32_t rc, map_t* ref);

void cycle_unblock(pony_actor_t* actor);

void cycle_updaterc(pony_actor_t* actor, uint32_t rc);

void cycle_ack(uint64_t token);

void cycle_finish(bool finish, bool stats);

#endif
