#include "stream.h"
#include "future.h"
#include <stdio.h>

struct scons{
  bool eos;
  encore_arg_t element;
  pony_type_t *type;
  stream_t *next;
};

void scons_trace(void *p){
  struct scons *scons = p;
  if(!scons->eos){
    pony_type_t *type = scons->type;
    if(type == ENCORE_ACTIVE){
      pony_traceactor(scons->element.p);
    } else if(type != ENCORE_PRIMITIVE){
      pony_traceobject(scons->element.p, type->trace);
    }
    pony_traceobject(scons->next, future_trace);
  }
}

pony_type_t scons_type = 
  {ID_SCONS, 
   sizeof(struct scons), 
   scons_trace, 
   NULL, 
   NULL,
   NULL,
   NULL
};

static struct scons *scons_mk(pony_type_t *type){
  struct scons *scons = pony_alloc(sizeof(struct scons));
  scons->eos = false;
  scons->element = (value_t) {.p = NULL};
  scons->type = type;
  scons->next = NULL;
  return scons;
}

// For debugging
static void scons_print(struct scons *scons){
  printf("struct scons@%p{\n", scons);
  printf("  eos     = %s\n", scons->eos? "true": "false");
  printf("  element = %ld\n", scons->element.i);
  printf("  type    = %p\n", scons->type);
  printf("  next    = %p\n", scons->next);
  printf("}\n");
}

stream_t *stream_mk(){
  return future_mk(&scons_type);
}

stream_t *stream_put(stream_t *s, encore_arg_t value, pony_type_t *type){
  future_t *fut = future_mk(&scons_type);
  struct scons *scons = scons_mk(type);
  scons->element = value;
  scons->next = fut;
  future_fulfil((future_t *)s, scons);
  return fut;
}

bool stream_eos(stream_t *s){
  struct scons *scons = future_get_actor((future_t *)s);
  return scons->eos;
}

encore_arg_t stream_get(stream_t *s){
  struct scons *scons = future_get_actor((future_t *)s);
  return scons->element;
}

stream_t *stream_get_next(stream_t *s){
  struct scons *scons = future_get_actor((future_t *)s);
  return scons->next;
}

void stream_close(stream_t *s){
  struct scons *scons = scons_mk(NULL);
  scons->eos = true;
  future_fulfil((future_t *)s, scons);
}
