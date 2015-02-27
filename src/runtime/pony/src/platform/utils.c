#include <platform/platform.h>

#if defined(PLATFORM_IS_WINDOWS)

#if defined(PLATFORM_IS_VISUAL_STUDIO)
#  define DELTA_EPOCH 116444736000000000Ui64
#elif defined(PLATFORM_IS_CLANG_OR_GCC)
#  define DELTA_EPOCH 116444736000000000ULL
#endif

uint32_t gettimeofday(struct timeval* tp, ...)
{
  FILETIME ft;
  SYSTEMTIME st;
  ULARGE_INTEGER d;

  GetSystemTime(&st);
  SystemTimeToFileTime(&st, &ft);

  d.LowPart = ft.dwLowDateTime;
  d.HighPart = ft.dwHighDateTime;

  tp->tv_sec = (long)((d.QuadPart - DELTA_EPOCH) / 10000000L);
  tp->tv_usec = (long)(st.wMilliseconds * 1000);

  return 0;
}

#endif
