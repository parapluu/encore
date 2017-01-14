#ifndef DTRACE_ENCORE_H
#define DTRACE_ENCORE_H

#if defined(USE_DYNAMIC_TRACE)

#include "encore_probes.h"

#define ENC_DTRACE_ENABLED(name)                         \
  ENCORE_##name##_ENABLED()
#define ENC_DTRACE0(name)                                \
  ENCORE_##name()
#define ENC_DTRACE1(name, a0)                            \
  ENCORE_##name(a0)
#define ENC_DTRACE2(name, a0, a1)                        \
  ENCORE_##name((a0), (a1))
#define ENC_DTRACE3(name, a0, a1, a2)                    \
  ENCORE_##name((a0), (a1), (a2))
#define ENC_DTRACE4(name, a0, a1, a2, a3)                \
  ENCORE_##name((a0), (a1), (a2), (a3))
#define ENC_DTRACE5(name, a0, a1, a2, a3, a4)            \
  ENCORE_##name((a0), (a1), (a2), (a3), (a4))

#else

#define ENC_DTRACE_ENABLED(name)                         0
#define ENC_DTRACE0(name)                                do {} while (0)
#define ENC_DTRACE1(name, a0)                            do {} while (0)
#define ENC_DTRACE2(name, a0, a1)                        do {} while (0)
#define ENC_DTRACE3(name, a0, a1, a2)                    do {} while (0)
#define ENC_DTRACE4(name, a0, a1, a2, a3)                do {} while (0)
#define ENC_DTRACE5(name, a0, a1, a2, a3, a4)            do {} while (0)

#endif

#endif
