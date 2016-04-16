#ifndef cycle_h
#define cycle_h

#include "gc.h"
#include <pony.h>
#include <platform.h>
#include <stdint.h>
#include <stdbool.h>

PONY_EXTERN_C_BEGIN

void cycle_create(pony_ctx_t* ctx, uint32_t min_deferred,
  uint32_t max_deferred, uint32_t conf_group);

void cycle_block(pony_ctx_t* ctx, pony_actor_t* actor, gc_t* gc);

void cycle_unblock(pony_ctx_t* ctx, pony_actor_t* actor);

void cycle_ack(pony_ctx_t* ctx, size_t token);

void cycle_terminate(pony_ctx_t* ctx);

bool is_cycle(pony_actor_t* actor);

PONY_EXTERN_C_END

#endif
