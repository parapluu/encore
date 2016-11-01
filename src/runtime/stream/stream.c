#include "stream.h"
#include "future.h"
#include <stdio.h>
#include <assert.h>

struct scons{
  bool eos;
  encore_arg_t element;
  pony_type_t *type;
  stream_t *next;
};

void scons_trace(pony_ctx_t *ctx, void *p)
{
  assert(p);
  struct scons *scons = p;
  if (!scons->eos) {
    pony_type_t *type = scons->type;
    if(type == ENCORE_ACTIVE){
      encore_trace_actor(ctx, scons->element.p);
    } else if(type != ENCORE_PRIMITIVE){
      encore_trace_object(ctx, scons->element.p, type->trace);
    }
    encore_trace_object(ctx, scons->next, future_trace);
  }
}

static pony_type_t scons_type = {
  .id = ID_SCONS,
  .size = sizeof(struct scons),
  .trace = scons_trace,
};

pony_type_t *get_scons_type()
{
  return &scons_type;
}

void stream_trace(pony_ctx_t *ctx, void *p)
{
  future_trace(ctx, p);
}

struct scons *scons_mk(pony_ctx_t *ctx, pony_type_t *type){
  struct scons *scons = encore_alloc(ctx, sizeof(struct scons));
  scons->eos = false;
  scons->element = (value_t) {.p = NULL};
  scons->type = type;
  scons->next = NULL;
  return scons;
}

// For debugging
__attribute__ ((unused))
static void scons_print(struct scons *scons){
  printf("struct scons@%p{\n", scons);
  printf("  eos     = %s\n", scons->eos? "true": "false");
  printf("  element = %ld\n", scons->element.i);
  printf("  type    = %p\n", scons->type);
  printf("  next    = %p\n", scons->next);
  printf("}\n");
}

stream_t *stream_mk(pony_ctx_t **ctx)
{
  return future_mk(ctx, &scons_type);
}

stream_t *stream_put(pony_ctx_t **ctx, stream_t *s, encore_arg_t value,
        pony_type_t *type)
{
  future_t *fut = future_mk(ctx, &scons_type);
  struct scons *scons = scons_mk(*ctx, type);
  scons->element = value;
  scons->next = fut;
  future_fulfil(ctx, (future_t *)s, (encore_arg_t){ .p = scons });
  return fut;
}

encore_arg_t stream_get(pony_ctx_t **ctx, stream_t *s)
{
  struct scons *scons = future_get_actor(ctx, (future_t *)s).p;
  return scons->element;
}

stream_t *stream_get_next(pony_ctx_t **ctx, stream_t *s)
{
  struct scons *scons = future_get_actor(ctx, (future_t *)s).p;
  return scons->next;
}

void stream_close(pony_ctx_t **ctx, stream_t *s)
{
  struct scons *scons = scons_mk(*ctx, NULL);
  scons->eos = true;
  future_fulfil(ctx, (future_t *)s, (encore_arg_t){ .p = scons });
}

bool stream_eos(pony_ctx_t **ctx, stream_t *s)
{
  struct scons *scons = future_get_actor(ctx, (future_t *)s).p;
  return scons->eos;
}

stream_t *stream_put_fut(pony_ctx_t **ctx, future_t* fut, stream_t *s,
                            encore_arg_t value, pony_type_t *type){
  struct scons *scons = scons_mk(*ctx,type);
  scons->element = value;
  scons->next = fut;
  future_fulfil(ctx,(future_t *)s, (encore_arg_t){ .p = scons });
  return fut;
}

bool scons_eos(pony_ctx_t **ctx, scons_t *scons){
  (void)ctx;
  return scons->eos;
}

encore_arg_t scons_element(pony_ctx_t **ctx, scons_t *scons){
  (void)ctx;
  return scons->element;
}

stream_t *scons_next(pony_ctx_t **ctx, scons_t *scons){
  (void)ctx;
  return scons->next;
}

scons_t *scons_tail(pony_ctx_t **ctx){
  struct scons *scons = scons_mk(*ctx, NULL);
  scons->eos = true;
  return scons;
}

scons_t *scons_put_fut(pony_ctx_t **ctx, stream_t *s,
                            encore_arg_t value, pony_type_t *type){
  struct scons *scons = scons_mk(*ctx,type);
  scons->element = value;
  scons->next = (future_t*)s;
  return scons;
}
