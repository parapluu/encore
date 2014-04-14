#ifndef map_h
#define map_h

#include <pony/pony.h>
#include <stdbool.h>

typedef struct map_t map_t;

typedef void (*map_cycle_fn)(void* arg, map_t* cycle);

map_t* map_alloc();

uint64_t map_size(map_t* map);

void map_free(map_t* map, bool nested);

void map_createactor(map_t** map, pony_actor_t* actor);

void map_receiveactor(map_t** map, pony_actor_t* actor);

bool map_receiveobject(map_t** map, pony_actor_t* owner, void* object);

void map_sendactor(map_t** map, map_t** to, pony_actor_t* actor);

bool map_sendobject(map_t** map, map_t** to, pony_actor_t* owner, void* object);

void map_markactor(map_t** map, map_t** to, pony_actor_t* actor);

bool map_markobject(map_t** map, map_t** to, pony_actor_t* owner, void* object);

bool map_sweep(map_t* map);

map_t* map_clonerefs(map_t* map);

bool map_receivelocal(map_t** map, void* object);

bool map_sendlocal(map_t** map, void* object, pony_trace_fn f);

void map_gc(map_t* map);

void map_receiverc(map_t** map, map_t* objects);

void map_sendrc(map_t** map);

bool map_contains(map_t* map, void* p);

void map_block(map_t** map, pony_actor_t* actor, uint32_t rc, map_t* ref);

void map_unblock(map_t* map, pony_actor_t* actor);

void map_updaterc(map_t* map, pony_actor_t* actor, uint32_t rc);

bool map_detectcycles(map_t* map, map_cycle_fn f, void* arg, uint64_t t);

void map_collectcycle(map_t* map);

void map_removecycle(map_t* map, map_t* cycle);

void map_clearmarks(map_t* map);

#endif
