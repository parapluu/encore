#include "stream.h"
#include "future.h"

struct scons{
  bool eos;
  pony_arg_t element;
  stream_t next;
};

static struct scons *scons_mk(){
  struct scons *scons = pony_alloc(sizeof(struct scons));
  scons->eos = false;
  scons->element = (value_t) {.p = NULL};
  scons->next = NULL;
  return scons;
}

const stream_t stream_mk(){
  return future_mk();
}

const stream_t stream_put(const stream_t s, pony_arg_t value){
  future_t *fut = future_mk();
  struct scons *scons = scons_mk();
  scons->element = value;
  scons->next = fut;
  future_fulfil(s, scons);
  return fut;
}

bool stream_eos(const stream_t s){
  struct scons *scons = future_get(s, NULL/* FIXME! */);
  return scons->eos;  
}

pony_arg_t stream_get(const stream_t s){
  struct scons *scons = future_get(s, NULL/* FIXME! */);
  return scons->element;
}

const stream_t stream_get_next(const stream_t s){
  struct scons *scons = future_get(s, NULL/* FIXME! */);
  return scons->next;
}

void stream_close(const stream_t s){
  struct scons *scons = scons_mk();
  scons->eos = true;
  future_fulfil(s, scons);
}