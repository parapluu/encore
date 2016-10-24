#include "party.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "closure.h"
#include "list.c"
#include <array.h>
#include <math.h>
#include <encore.h>

typedef struct fmap_s fmap_s;
typedef par_t* (*fmapfn)(par_t*, fmap_s*);

// TODO: add JOIN_PAR
enum PTAG { EMPTY_PAR, VALUE_PAR, FUTURE_PAR, FUTUREPAR_PAR, PAR_PAR, ARRAY_PAR};

typedef struct EMPTY_PARs {} EMPTY_PARs;
typedef struct VALUE_PARs { value_t val; char padding[8];} VALUE_PARs;
typedef struct FUTURE_PARs { future_t* fut; } FUTURE_PARs;
typedef struct FUTUREPAR_PARs { future_t* fut; } FUTUREPAR_PARs;
typedef struct PAR_PARs { struct par_t* left; struct par_t* right; } PAR_PARs;
typedef struct JOIN_PARs { struct par_t* join; } JOIN_PARs;
typedef struct ARRAY_PARs { struct array_t* array; } ARRAY_PARs;

struct par_t {
    enum PTAG tag;
    pony_type_t* rtype;
    union ParU {
      EMPTY_PARs s;
      VALUE_PARs v;
      FUTURE_PARs f;
      PAR_PARs p;
      FUTUREPAR_PARs fp;
      JOIN_PARs j;
      ARRAY_PARs a;
    } data;
};

pony_type_t party_type =
{
  .id=ID_PARTY,
  .size=sizeof(struct par_t),
  .trace=party_trace
};

#define get_rtype(x) (x)->rtype

// MACRO for setting the parallel collection. Needs gcc-4.9
/* #define set_par(elem1, elem2, par) \ */
/*   _Generic((elem1),                \ */
/*   future_t*: set_par_future,       \ */
/*   par_t*: set_par_par,             \ */
/*   array_t*: set_par_array,         \ */
/*   encore_arg_t: set_par_value)(elem1, elem2, par) */

static inline void set_par_value(encore_arg_t val,
                                 void * __attribute__((unused))null,
                                 par_t * const par){
  assert(par->tag == VALUE_PAR);
  par->data.v.val = val;
}

static inline void set_par_future(future_t * const fut,
                                  void * __attribute__((unused)) null,
                                  par_t *par){
  assert(par->tag == FUTURE_PAR);
  par->data.f.fut = fut;
}

static inline void set_par_future_par(future_t * const fut,
                                      void * __attribute__((unused)) null,
                                      par_t *par){
  assert(par->tag == FUTUREPAR_PAR);
  par->data.fp.fut = fut;
}

static inline void set_par_par(par_t * const rpar, par_t * const lpar,
                               par_t * const par){
  switch(par->tag){
  case PAR_PAR: {
      par->data.p.left = lpar;
      par->data.p.right = rpar;
      break;
  }
  default: assert(0); exit(-1);
  }
}

static inline void set_par_array(array_t* const arr,
                   void * __attribute__((unused)) null,
                   par_t* const p){
  assert(p->tag == ARRAY_PAR);
  p->data.a.array = arr;
}

static inline void trace_array_par(pony_ctx_t *ctx, par_t* obj){
  array_t* ar = obj->data.a.array;
  if(obj->rtype == ENCORE_PRIMITIVE){
    for(size_t i = 0; i<array_size(ar); i++){
      void* val = array_get(ar, i).p;
      encore_trace_actor(ctx, val);
    }
  }else{
    pony_trace_fn trace_fn = obj->rtype->trace;
    for(size_t i = 0; i<array_size(ar); i++){
      void* val = array_get(ar, i).p;
      encore_trace_object(ctx, val, trace_fn);
    }
  }
}

void party_trace(pony_ctx_t* ctx, void* p){
  assert(p);
  par_t *obj = p;
  if(obj->rtype == ENCORE_ACTIVE){
    if(obj->tag == VALUE_PAR)
      encore_trace_actor(ctx, (pony_actor_t*) obj->data.v.val.p);
    else
      trace_array_par(ctx, obj);
  }else if(obj->rtype != ENCORE_PRIMITIVE){
    switch(obj->tag){
    case EMPTY_PAR: break;
    case VALUE_PAR: {
      encore_trace_object(ctx, obj->data.v.val.p, obj->rtype->trace);
      break;
    }
    case FUTURE_PAR: {
      encore_trace_object(ctx, obj->data.f.fut, obj->rtype->trace);
      break;
    }
    case PAR_PAR: {
      party_trace(ctx, obj->data.p.left);
      party_trace(ctx, obj->data.p.right);
      break;
    }
    case FUTUREPAR_PAR: {
      encore_trace_object(ctx, obj->data.fp.fut, obj->rtype->trace);
      break;
    }
    case ARRAY_PAR: {
      trace_array_par(ctx, obj);
      break;
    }
    /* case JOIN_PAR: { */
    /*   party_trace(obj->data.j.join); */
    /*   break; */
    /* } */
    }
  }
}

struct fmap_s{
  closure_t* fn;
  pony_type_t const * rtype;
};

/*
 * Parallel constructors
 */

static par_t* init_par(pony_ctx_t **ctx, enum PTAG tag, pony_type_t const * const rtype){
  par_t* res = encore_alloc(*ctx, sizeof* res);
  res->tag = tag;
  res->rtype = rtype;
  return res;
}

par_t* new_par_empty(pony_ctx_t **ctx, pony_type_t const * const rtype){
  return init_par(ctx, EMPTY_PAR, rtype);
}

par_t* new_par_v(pony_ctx_t **ctx, encore_arg_t val, pony_type_t const * const rtype){
  par_t* p = init_par(ctx, VALUE_PAR, rtype);
  set_par_value(val, NULL, p);
  return p;
}

par_t* new_par_f(pony_ctx_t **ctx, future_t* f, pony_type_t const * const rtype){
  par_t* p = init_par(ctx, FUTURE_PAR, rtype);
  set_par_future(f, NULL, p);
  return p;
}

par_t* new_par_p(pony_ctx_t **ctx, par_t* p1, par_t* p2,
                 pony_type_t const * const rtype){
  par_t* p = init_par(ctx, PAR_PAR, rtype);
  set_par_par(p1, p2, p);
  return p;
}

par_t* new_par_fp(pony_ctx_t **ctx, future_t* f, pony_type_t const * const rtype){
  par_t* p = init_par(ctx, FUTUREPAR_PAR, rtype);
  set_par_future_par(f, NULL, p);
  return p;
}

par_t* new_par_array(pony_ctx_t **ctx, array_t* arr, pony_type_t const * const rtype){
  par_t* p = init_par(ctx, ARRAY_PAR, rtype);
  set_par_array(arr, NULL, p);
  return p;
}

// TODO: enable once we can create J
/* par_t* new_par_join(par_t* const p, pony_type_t const * const rtype){ */
/*   par_t* par = init_par(JOIN_PAR, rtype); */
/*   par->data.join = p; */
/*   return par; */
/* } */

//---------------------------------------
// SEQUENCE COMBINATOR
//---------------------------------------

// Forward declaration
static par_t* fmap(pony_ctx_t **ctx, closure_t * const f, par_t * in,
                   pony_type_t const * const rtype);

static value_t fmap_party_closure(pony_ctx_t **ctx,
                                  pony_type_t** __attribute__ ((unused)) rType,
                                  value_t args[], void * const env){
  par_t *p = (par_t*)args[0].p;
  fmap_s *fm = env;
  return (value_t){.p = fmap(ctx, fm->fn, p, get_rtype(fm))};
}

static inline par_t* fmap_run_fp(pony_ctx_t **ctx, par_t* in, fmap_s* const f){
  closure_t *cp = closure_mk(ctx, fmap_party_closure, f, NULL, NULL);
  future_t *fut = future_chain_actor(ctx, in->data.fp.fut, &party_type, cp);
  return new_par_fp(ctx, fut, &future_type);
}

static inline par_t* fmap_run_v(pony_ctx_t **ctx, par_t* in, fmap_s* const f){
  value_t v = closure_call(ctx, f->fn, (value_t[]){in->data.v.val});
  return new_par_v(ctx, v, get_rtype(f));
}

static inline par_t* fmap_run_f(pony_ctx_t **ctx, par_t * in, fmap_s * const f){
  future_t* chained_fut = future_chain_actor(ctx, in->data.f.fut, get_rtype(f), f->fn);
  return new_par_f(ctx, chained_fut, &future_type);
}

static inline par_t* fmap_run_array(pony_ctx_t **ctx, par_t * in, fmap_s * const f){
  array_t* old_array = in->data.a.array;
  size_t size = array_size(old_array);
  pony_type_t* type = get_rtype(f);
  array_t* new_array = array_mk(ctx, size, type);
  closure_t* clos = f->fn;

  for(size_t i = 0; i < size; i++){
    value_t value = array_get(old_array, i);
    value_t new_value = closure_call(ctx, clos, (value_t[]){value});
    array_set(new_array, i, new_value);
  }

  return new_par_array(ctx, new_array, type);
}

// WARNING:
// this function is only used for fmap J, which guarantees
// that args is of: {.p = par_t* }
/* static encore_arg_t fmap_fmap_closure(value_t args[], void* const env){ */
/*   fmap_s* fm = env; */
/*   encore_arg_t arg = args[0]; */
/*   return (encore_arg_t) {.p = fmap(fm->fn, (par_t*) arg.p, fm->rtype)}; */
/* } */

/* static inline par_t* fmap_run_j(par_t* const in, fmap_s* const f){ */
/*   fmap_s* fm = (fmap_s*)encore_alloc(sizeof* fm); */
/*   *fm = (fmap_s){.fn=f->fn, .rtype = f->rtype}; */
/*   closure_t* clos = closure_mk(fmap_fmap_closure, fm, NULL); */
/*   par_t* p = fmap(clos, in->data.join, get_rtype(fm)); */
/*   return new_par_join(p, get_rtype(f)); */
/* } */

/**
 *  fmap: (a -> b) -> Par a -> Par b
 *
 *  @param f Closure function to be called
 *  @param in Parallel collection data structure to call \p f on
 *  @param rtype The returned runtime type (ENCORE_PRIMITIVE or ENCORE_ACTIVE)
 *  @return a pointer to a new parallel collection of \p rtype runtime type
 */

// TODO: enable JOIN_PAR once it is added to the language
static par_t* fmap(pony_ctx_t** ctx, closure_t* const f, par_t* in,
                   pony_type_t const * const rtype){
  fmap_s *fm = (fmap_s*) encore_alloc(*ctx, sizeof* fm);
  *fm = (fmap_s){.fn = f, .rtype=rtype};
  switch(in->tag){
  case EMPTY_PAR: return new_par_empty(ctx, rtype);
  case VALUE_PAR: return fmap_run_v(ctx, in, fm);
  case FUTURE_PAR: return fmap_run_f(ctx, in, fm);
  case PAR_PAR: {
    // TODO: may consume all the stack
    par_t* left = fmap(ctx, f, in->data.p.left, rtype);
    par_t* right = fmap(ctx, f, in->data.p.right, rtype);
    return new_par_p(ctx, left, right, &party_type);
  }
  case FUTUREPAR_PAR: return fmap_run_fp(ctx, in, fm);
  case ARRAY_PAR: return fmap_run_array(ctx, in, fm);
  /* case JOIN_PAR: return fmap_run_j(in, fm); */
  default: exit(-1);
  }
}

par_t* party_sequence(pony_ctx_t **ctx, par_t* p, closure_t* const f,
                      pony_type_t const * const rtype){
  return fmap(ctx, f, p, rtype);
}

//---------------------------------------
// JOIN COMBINATOR
//---------------------------------------

// Forward declaration of party_join
par_t* party_join(pony_ctx_t **ctx, par_t* const p);

static inline par_t* party_join_v(pony_ctx_t ** ctx, par_t* const p){
  (void) ctx;
  return ((par_t*)p->data.v.val.p);
}

static inline par_t* party_join_p(pony_ctx_t **ctx, par_t* const p){
  par_t *left = p->data.p.left;
  par_t *right = p->data.p.right;

  if(left->tag == EMPTY_PAR){
    return party_join(ctx, right);
  }else if(right->tag == EMPTY_PAR){
    return party_join(ctx, left);
  }else{
    par_t* pleft = party_join(ctx, left);
    par_t* pright = party_join(ctx, right);
    return new_par_p(ctx, pleft, pright, &party_type);
  }
}

static value_t party_join_fp_closure(pony_ctx_t **ctx,
                                     pony_type_t** __attribute__ ((unused)) rType,
                                     value_t args[],
                                     void* __attribute__ ((unused)) env){
  par_t* const p = (par_t*)args[0].p;
  return (value_t){.p = party_join(ctx, p)};
}

static inline par_t* party_join_fp(pony_ctx_t **ctx, par_t* const p){
  future_t* const fut = p->data.fp.fut;
  closure_t* const clos = closure_mk(ctx, party_join_fp_closure, NULL, NULL, NULL);
  future_t* const chained_fut = future_chain_actor(ctx, fut, get_rtype(p), clos);
  return new_par_fp(ctx, chained_fut, get_rtype(p));
}

static value_t closure_join(pony_ctx_t **ctx,
                            pony_type_t** __attribute__ ((unused)) rType,
                            value_t val[],
                            void* __attribute__ ((unused)) env){
  return (value_t){.p = party_join(ctx, val[0].p)};
}

static inline par_t* party_join_array(pony_ctx_t **ctx, par_t* const p){
  closure_t* clos = closure_mk(ctx, closure_join, NULL, party_trace, NULL);
  return party_sequence(ctx, p, clos, get_rtype(p));
}

par_t* party_join(pony_ctx_t **ctx, par_t* const p){
  switch(p->tag){
  case EMPTY_PAR: return p;
  case VALUE_PAR: return party_join_v(ctx, p);
  case FUTURE_PAR: return new_par_fp(ctx, p->data.f.fut, get_rtype(p));
  case PAR_PAR: return party_join_p(ctx, p);
  case FUTUREPAR_PAR: return party_join_fp(ctx, p);
  case ARRAY_PAR: return party_join_array(ctx, p);
  /* case JOIN_PAR: return party_join(party_join(p->data.join)); */
  default: exit(-1);
  }
}

//----------------------------------------
// EXTRACT COMBINATOR
//----------------------------------------

static inline list_t* extract_helper(pony_ctx_t **ctx, list_t * const list, par_t * const p){
  switch(p->tag){
  case EMPTY_PAR: return list;
  case VALUE_PAR: return list_push(list, p->data.v.val);
  case FUTURE_PAR: {
    future_t *fut = p->data.f.fut;
    value_t val = future_get_actor(ctx, fut);
    return list_push(list, val);
  }
  case PAR_PAR: {
    par_t *left = p->data.p.left;
    par_t *right = p->data.p.right;
    list_t* tmp_list = extract_helper(ctx, list, left);
    return extract_helper(ctx, tmp_list, right);
  }
  case FUTUREPAR_PAR: {
    future_t *fut = p->data.f.fut;
    par_t* par = future_get_actor(ctx, fut).p;
    return extract_helper(ctx, list, par);
  }
  case ARRAY_PAR: {
    array_t* ar = p->data.a.array;
    size_t size = array_size(ar);
    list_t* new_list = list;
    for(size_t i=0; i<size; i++){
      value_t value = array_get(ar, i);
      new_list = list_push(new_list, value);
    }
    return new_list;
  }
  default: exit(-1);
  }
}

static inline array_t* list_to_array(pony_ctx_t **ctx, list_t* const list,
                                     pony_type_t const * const type){
  size_t size = list_length(list);
  array_t* arr = array_mk(ctx, size, type);
  list_t* temp_list = list_index(list, 0);

  // TODO: If the list is too big, distribute work using tasks
  for(size_t i=0; i<size; i++) {
    array_set(arr, i, list_data(temp_list));
    temp_list = list_index(temp_list, 1);
  }
  return arr;
}

array_t* party_extract(pony_ctx_t **ctx,
                       par_t * const p, pony_type_t const * const type){
  list_t *list = NULL;
  list_t * const tmp_list = extract_helper(ctx, list, p);

  return list_to_array(ctx, tmp_list, type);
}

//----------------------------------------
// EACH COMBINATOR
//----------------------------------------

#define SPLIT_THRESHOLD 1000
#define MARGIN 1.3

static void build_party_tree(pony_ctx_t **ctx, par_t** root, par_t* node){
  if(*root == NULL)
    *root = node;
  else
    *root = new_par_p(ctx, *root, node, get_rtype(*root));
}


// TODO: Fix tasks. Tasks were removed from the language
/* #ifdef PARTY_ARRAY_PARALLEL */
/* typedef struct env_par { */
/*   par_t* p; */
/* } env_par; */

/* static value_t id_as_task(void* env, void* __attribute__((unused)) null){ */
/*   par_t* par = ((env_par*)env)->p; */
/*   return (value_t){.p = par}; */
/* } */
/* #endif */

par_t* party_each(pony_ctx_t **ctx, array_t* const ar){
  size_t start, end;
  par_t* root = NULL;

  size_t size = array_size(ar);
  pony_type_t* type = array_get_type(ar);
  size_t groups = (size_t) ceil(size / (MARGIN * SPLIT_THRESHOLD));

  for(size_t i=0; i<groups; i++){
    start = i * SPLIT_THRESHOLD;
    end = (i+1 == groups) ? size+1 : (i+1) * SPLIT_THRESHOLD;

    array_t* const chunk = array_get_chunk(ctx, start, end, ar);
    par_t* par = new_par_array(ctx, chunk, type);
    // TODO: Fix tasks. Tasks were removed from the language
/* #ifdef PARTY_ARRAY_PARALLEL */
/*     future_t* fut = future_mk(ctx, array_get_type(ar)); */
/*     env_par* env = encore_alloc(ctx, sizeof* env); */
/*     env->p = par; */
/*     encore_task_s* task = task_mk(ctx, id_as_task, env, NULL, NULL); */
/*     task_attach_fut(task, fut); */
/*     task_schedule(task); */

/*     pony_gc_send(ctx); */
/*     encore_trace_object(ctx, fut, future_type.trace); */
/*     encore_trace_object(ctx, task, NULL); */
/*     pony_send_done(ctx); */

/*     par = new_par_fp(ctx, fut, type); */
/* #endif */
    build_party_tree(ctx, &root, par);
  }
  return root;
}
