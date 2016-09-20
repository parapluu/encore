#ifndef sched_cpu_h
#define sched_cpu_h

#include "scheduler.h"
#include <stdint.h>
#include <stdbool.h>
#include <platform.h>

PONY_EXTERN_C_BEGIN

uint32_t cpu_count();

void cpu_assign(uint32_t count, scheduler_t* scheduler);

void cpu_affinity(uint32_t cpu);

void cpu_core_pause(uint64_t tsc, uint64_t tsc2, bool yield);

uint64_t cpu_tick();

PONY_EXTERN_C_END

#endif
