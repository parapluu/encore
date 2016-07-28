#ifndef __adt_option_h__
#define __adt_option_h__

#include <pony.h>
#include <encore.h>

typedef enum {JUST, NOTHING} option_tag;
typedef struct option_t option_t;

extern pony_type_t option_type;

void option_trace(pony_ctx_t*, void *);

struct option_t {
  union {
    encore_arg_t_content;
    encore_arg_t val;
  };
  option_tag tag;
  pony_type_t *type;
};

extern option_t DEFAULT_NOTHING;

option_t *option_mk(pony_ctx_t**, option_tag, encore_arg_t, pony_type_t*);

#endif
