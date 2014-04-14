#ifndef UPDATER_H
#define UPDATER_H

#include <pony/pony.h>
#include <stdint.h>

pony_actor_t* updater_create(uint64_t index, uint64_t size);

void updater_apply(pony_actor_t* updater, uint64_t size, uint64_t* data);

void updater_done(pony_actor_t* updater, pony_actor_t* main);

#endif
