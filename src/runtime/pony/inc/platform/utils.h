#ifndef PLATFORM_UTILS_H
#define PLATFORM_UTILS_H

#if defined(PLATFORM_IS_WINDOWS)
#  define VLA(TYPE, NAME, SIZE) TYPE* NAME = (TYPE*) alloca(\
            SIZE*sizeof(TYPE))

// Windows has equivalent timeval and timezone definitions.
// timezone is obsolete on linux, will be ignored in this gettimeofday
uint32_t gettimeofday(struct timeval* tv, ...);
#else
#  include <sys/time.h>
#  define VLA(TYPE, NAME, SIZE) TYPE NAME[SIZE]
#endif

#endif
