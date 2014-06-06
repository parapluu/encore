#ifndef actor_h
#define actor_h

#include "map.h"
#include <pony/pony.h>
#include <stdint.h>
#include <stdbool.h>

enum
{
  ACTORMSG_RC = UINT64_MAX - 2,
  ACTORMSG_CONF
};

bool actor_run(pony_actor_t* actor, uint32_t thread,
  uint64_t* app_msgs, uint64_t* rc_msgs);

void actor_destroy(pony_actor_t* actor, map_t* cycle);

void actor_sendv(pony_actor_t* to, uint64_t id, int argc, pony_arg_t* argv);

pony_actor_t* actor_create(pony_actor_type_t* type);

pony_actor_t* actor_create_stage_two(pony_actor_type_t* type, pony_actor_t* actor);

void actor_disablecd(bool state);

#endif
