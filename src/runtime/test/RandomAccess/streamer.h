#ifndef STREAMER_H
#define STREAMER_H

#include "arrays.h"

pony_actor_t* streamer_create(pony_actor_t* main, array_actor_t* updaters,
  uint64_t size, uint64_t chunk, uint64_t seed);

void streamer_apply(pony_actor_t* streamer, uint64_t iterate);

#endif
