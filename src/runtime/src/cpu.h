#ifndef cpu_h
#define cpu_h

#include <stdint.h>
#include <stdbool.h>

void cpu_count(uint32_t* physical, uint32_t* logical);

bool cpu_physical(uint32_t cpu);

void cpu_affinity(uint32_t cpu);

#endif
