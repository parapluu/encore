#if defined(__linux__)
#define _GNU_SOURCE
#include <sched.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#elif defined(__APPLE__)
#include <sys/types.h>
#include <sys/sysctl.h>
#include <mach/mach.h>
#include <mach/thread_policy.h>
#endif

#include <pthread.h>
#include "cpu.h"

#if defined(__APPLE__)
uint32_t property(const char* key)
{
  int value;
  size_t len = sizeof(int);
  sysctlbyname(key, &value, &len, NULL, 0);
  return value;
}
#endif

void cpu_count(uint32_t* physical, uint32_t* logical)
{
#if defined(__linux__)
  int count = sysconf(_SC_NPROCESSORS_ONLN);
  int res = 0;

  for(int i = 0; i < count; i++)
  {
    if(cpu_physical(i)) res++;
  }

  *physical = res;
  *logical = count;
#elif defined(__APPLE__)
  *physical = property("hw.physicalcpu");
  *logical = property("hw.logicalcpu");
#else
  *physical = 0;
  *logical = 0;
#endif
}

bool cpu_physical(uint32_t cpu)
{
#if defined(__linux__)
  char file[FILENAME_MAX];
  snprintf(file, FILENAME_MAX,
    "/sys/devices/system/cpu/cpu%d/topology/thread_siblings_list", cpu);

  FILE* fp = fopen(file, "r");

  if(fp != NULL)
  {
    char name[16];
    size_t len = fread(name, 1, 15, fp);
    name[len] = '\0';
    fclose( fp );

    if(cpu != atoi(name)) { return false; }
  }
#endif

  return true;
}

void cpu_affinity(uint32_t cpu)
{
#if defined(__linux__)
  cpu_set_t set;
  CPU_ZERO(&set);
  CPU_SET(cpu, &set);
  sched_setaffinity(0, 1, &set);
#elif defined(__APPLE__)
  thread_affinity_policy_data_t policy;
  policy.affinity_tag = cpu;
  thread_policy_set(mach_thread_self(), THREAD_AFFINITY_POLICY,
    (thread_policy_t)&policy, THREAD_AFFINITY_POLICY_COUNT);
#endif
}

uint64_t cpu_rdtsc()
{
#ifdef __clang__
  return __builtin_readcyclecounter();
#else
  return __builtin_ia32_rdtsc();
#endif
}

/**
 * Only nanosleep if sufficient cycles have elapsed.
 */
bool cpu_core_pause(uint64_t tsc)
{
  uint64_t tsc2 = cpu_rdtsc();

  // 10m cycles is about 3ms
  if((tsc2 - tsc) < 10000000)
    return false;

  struct timespec ts = {0, 0};
  nanosleep(&ts, NULL);
  return true;
}
