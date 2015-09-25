#ifndef __adt_option_h__
#define __adt_option_h__

#include <pony.h>
#include <encore.h>

typedef enum {JUST, NOTHING} option_tag;
typedef struct option_t option_t;

struct option_t {
  union {
    encore_arg_t_content;
    encore_arg_t val;
  };
  option_tag tag;
};

extern option_t DEFAULT_NOTHING;

#endif
