#ifndef UPDATER_H
#define UPDATER_H

pony_actor_t* updater_create(pony_actor_t* main, array_actor_t* updaters,
  uint64_t index, uint64_t size, uint64_t chunk, uint64_t seed);

void updater_apply(pony_actor_t* updater, uint64_t iterate);

void updater_receive(pony_actor_t* updater, array_uint64_t* data);

void updater_done(pony_actor_t* updater);

#endif
