#include "party.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "closure.h"
#include <array.h>
#include <math.h>
#include <encore.h>
#include "structure.h"
#include "list.c"
#include "set.h"

typedef struct fmap_s fmap_s;
typedef par_t* (*fmapfn)(par_t*, fmap_s*);

typedef struct EMPTY_PARs {} EMPTY_PARs;
typedef struct VALUE_PARs { value_t val; char padding[8];} VALUE_PARs;
typedef struct FUTURE_PARs { future_t* fut; } FUTURE_PARs;
typedef struct FUTUREPAR_PARs { future_t* fut; } FUTUREPAR_PARs;
typedef struct PAR_PARs { struct par_t* left; struct par_t* right; } PAR_PARs;
typedef struct ARRAY_PARs { struct array_t* array; } ARRAY_PARs;

// Auxiliary ds to de-struct the size of a ParT.
typedef struct psize_s {
  bool pending;
  size_t size;
} psize_s;

struct par_t {
  enum PTAG tag;
  pony_type_t* rtype;
  uint64_t size;
  union ParU {
    EMPTY_PARs s;
    VALUE_PARs v;
    FUTURE_PARs f;
    PAR_PARs p;
    FUTUREPAR_PARs fp;
    ARRAY_PARs a;
  } data;
};

// forward declaration
static inline par_t* party_promise_await_on_futures(pony_ctx_t **ctx,
                                                    par_t *par,
                                                    closure_t *call,
                                                    closure_t *cmp);

pony_type_t party_type =
{
  .id=ID_PARTY,
  .size=sizeof(struct par_t),
  .trace=party_trace
};

PTAG party_tag(par_t const * const p){
  return p->tag;
}

encore_arg_t party_get_v(par_t const * const p){
  return p->data.v.val;
}

future_t* party_get_fut(par_t const * const p){
  return p->data.f.fut;
}

par_t* party_get_parleft(par_t const * const p){
    return p->data.p.left;
}

par_t* party_get_parright(par_t const * const p){
    return p->data.p.right;
}

future_t* party_get_futpar(par_t const * const p){
  return p->data.fp.fut;
}

array_t* party_get_array(par_t const * const p){
  return p->data.a.array;
}

pony_type_t* party_get_type(par_t * const p){
  return p->rtype;
}

#define get_rtype(x) (x)->rtype

static inline void set_par_value(encore_arg_t val,
                                 __attribute__((unused)) void* null,
                                 par_t * const par){
  assert(par->tag == VALUE_PAR);
  par->data.v.val = val;
}

static inline void set_par_future(future_t * const fut,
                                  __attribute__((unused)) void* null,
                                  par_t *par){
  assert(par->tag == FUTURE_PAR);
  par->data.f.fut = fut;
}

static inline void set_par_future_par(future_t * const fut,
                                      __attribute__((unused)) void* null,
                                      par_t *par){
  assert(par->tag == FUTUREPAR_PAR);
  par->data.fp.fut = fut;
}

static inline void set_par_par(par_t * const lpar,
                               par_t * const rpar,
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
                                 __attribute__((unused)) void* null,
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
      encore_trace_object(ctx, obj->data.f.fut, future_trace);
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
    }
  }
}

struct fmap_s{
  closure_t* fn;
  pony_type_t const * rtype;
};

static inline psize_s party_get_size(par_t const * const p){
  uint64_t size = p->size;
  // if pending is 0, it means it contains pending FutPar t,
  // therefore pending = true
  bool pending = !__builtin_clzl(size);
  size = (size << 1) >> 1;
  return (psize_s) {.size = size, .pending = pending};
}

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
  par_t *p = init_par(ctx, EMPTY_PAR, rtype);
  p->size = 0;

  return p;
}

par_t* new_par_v(pony_ctx_t **ctx, encore_arg_t val, pony_type_t const * const rtype){
  par_t* p = init_par(ctx, VALUE_PAR, rtype);
  set_par_value(val, NULL, p);
  p->size = 1;
  return p;
}

par_t* new_par_f(pony_ctx_t **ctx, future_t* f, pony_type_t const * const rtype){
  par_t* p = init_par(ctx, FUTURE_PAR, rtype);
  set_par_future(f, NULL, p);
  p->size = 1;
  return p;
}

par_t* new_par_p(pony_ctx_t **ctx, par_t* p1, par_t* p2,
                 pony_type_t const * const rtype){
  if (p1->tag == EMPTY_PAR) {
    return p2;
  } else if (p2->tag == EMPTY_PAR) {
    return p1;
  } else {
    par_t* p = init_par(ctx, PAR_PAR, rtype);
    set_par_par(p1, p2, p);

    // setting the new size
    psize_s s1 = party_get_size(p1);
    psize_s s2 = party_get_size(p2);
    bool pending = s1.pending || s2.pending;
    uint64_t size = s1.size + s2.size;
    uint64_t one = 1;
    p->size = !pending ? size : ((one << 63) | size);

    return p;
  }
}

par_t* new_par_fp(pony_ctx_t **ctx, future_t* f, pony_type_t const * const rtype){
  par_t* p = init_par(ctx, FUTUREPAR_PAR, rtype);
  set_par_future_par(f, NULL, p);
  uint64_t one = 1;
  p->size = one << 63;
  return p;
}

par_t* new_par_array(pony_ctx_t **ctx, array_t* arr, pony_type_t const * const rtype){
  par_t* p = init_par(ctx, ARRAY_PAR, rtype);
  set_par_array(arr, NULL, p);
  p->size = array_size(arr);
  return p;
}

//---------------------------------------
// SEQUENCE COMBINATOR
//---------------------------------------

// Forward declaration
static par_t* fmap(pony_ctx_t **ctx, closure_t * const f, par_t * in,
                   pony_type_t const * const rtype);

static value_t fmap_party_closure(pony_ctx_t **ctx,
                                  __attribute__ ((unused)) pony_type_t**  rType,
                                  value_t args[],
                                  void * const env){
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

/**
 *  fmap: (a -> b) -> Par a -> Par b
 *
 *  @param f Closure function to be called
 *  @param in Parallel collection data structure to call \p f on
 *  @param rtype The returned runtime type (ENCORE_PRIMITIVE or ENCORE_ACTIVE)
 *  @return a pointer to a new parallel collection of \p rtype runtime type
 */

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
                                     __attribute__ ((unused))pony_type_t**  rType,
                                     value_t args[],
                                     __attribute__ ((unused)) void* env){
  par_t* const p = (par_t*)args[0].p;
  return (value_t){.p = party_join(ctx, p)};
}

static inline par_t* party_join_fp(pony_ctx_t **ctx, par_t* const p){
  future_t* const fut = p->data.fp.fut;
  closure_t* const clos = closure_mk(ctx, party_join_fp_closure, NULL, NULL, NULL);
  future_t* const chained_fut = future_chain_actor(ctx, fut, get_rtype(p), clos);
  return new_par_fp(ctx, chained_fut, get_rtype(p));
}

static inline par_t* party_join_array(pony_ctx_t **ctx, par_t* const p){
  pony_type_t *type = get_rtype(p);
  assert(type == &party_type);

  array_t *ar = party_get_array(p);
  size_t size = array_size(ar);
  par_t *new_p = new_par_empty(ctx, get_rtype(p));
  for (size_t i = 0; i < size ; ++i){
    par_t *v = array_get(ar, i).p;
    new_p = new_par_p(ctx, new_p, v, type);
  }
  return new_p;
}

par_t* party_join(pony_ctx_t **ctx, par_t* const p){
  switch(p->tag){
  case EMPTY_PAR: return p;
  case VALUE_PAR: return party_join_v(ctx, p);
  case FUTURE_PAR: return new_par_fp(ctx, p->data.f.fut, get_rtype(p));
  case PAR_PAR: return party_join_p(ctx, p);
  case FUTUREPAR_PAR: return party_join_fp(ctx, p);
  case ARRAY_PAR: return party_join_array(ctx, p);
  default: exit(-1);
  }
}

//----------------------------------------
// EXTRACT COMBINATOR
//----------------------------------------

static inline size_t party_get_final_size(pony_ctx_t **ctx, par_t const * p)
{
  psize_s s = party_get_size(p);
  if (!s.pending) {
    return s.size;
  } else {
    list_t *tmp_lst = NULL;
    size_t i = 0;
    while(p){
      switch(p->tag){
      case EMPTY_PAR: {
        tmp_lst = list_pop(tmp_lst, (value_t*)&p);
        break;
      }
      case VALUE_PAR: {
        ++i;
        tmp_lst = list_pop(tmp_lst, (value_t*)&p);
        break;
      }
      case FUTURE_PAR: {
        ++i;
        tmp_lst = list_pop(tmp_lst, (value_t*)&p);
        break;
      }
      case PAR_PAR: {
        par_t *left = party_get_parleft(p);
        par_t *right = party_get_parright(p);
        tmp_lst = list_push(tmp_lst, (value_t) { .p = right });
        p = left;
        break;
      }
      case FUTUREPAR_PAR: {
        future_t *futpar = party_get_futpar(p);
        future_await(ctx, futpar);
        p = future_get_actor(ctx, futpar).p;
        break;
      }
      case ARRAY_PAR: {
        array_t* ar_p = party_get_array(p);
        size_t size_p = array_size(ar_p);
        i += size_p;
        break;
      }
      default: exit(-1);
      }
    }
    return i;
  }
}

static inline array_t* party_to_array(pony_ctx_t **ctx,
                                      par_t const * p,
                                      size_t ar_size,
                                      pony_type_t *type)
{
  array_t *ar = array_mk(ctx, ar_size, type);

  if (ar_size > 0){
    list_t *tmp_lst = NULL;
    size_t i = 0;
    while(p){
      assert(i < ar_size);
      switch(p->tag){
      case EMPTY_PAR: {
        tmp_lst = list_pop(tmp_lst, (value_t*)&p);
        break;
      }
      case VALUE_PAR: {
        array_set(ar, i, party_get_v(p));
        tmp_lst = list_pop(tmp_lst, (value_t*)&p);
        ++i;
        break;
      }
      case FUTURE_PAR: {
        future_t *fut = party_get_fut(p);
        value_t v = future_get_actor(ctx, fut);
        array_set(ar, i, v);
        tmp_lst = list_pop(tmp_lst, (value_t*)&p);
        ++i;
        break;
      }
      case PAR_PAR: {
        par_t *left = party_get_parleft(p);
        par_t *right = party_get_parright(p);
        tmp_lst = list_push(tmp_lst, (value_t) { .p = right });
        p = left;
        break;
      }
      case FUTUREPAR_PAR: {
        future_t *futpar = party_get_futpar(p);
        p = future_get_actor(ctx, futpar).p;
        break;
      }
      case ARRAY_PAR: {
        array_t* p_ar = party_get_array(p);
        size_t size_p = array_size(p_ar);
        for(size_t j = 0; j < size_p; ++j){
          value_t value = array_get(p_ar, j);
          array_set(ar, i, value);
          ++i;
        }
        tmp_lst = list_pop(tmp_lst, (value_t*)&p);
        break;
      }
      default: exit(-1);
      }
    }
  }
  return ar;
}

array_t* party_extract(pony_ctx_t **ctx,
                       par_t * const par,
                       pony_type_t const *type)
{
  size_t size = party_get_final_size(ctx, par);
  return party_to_array(ctx, par, size, type);
}

//----------------------------------------
// EACH COMBINATOR
//----------------------------------------

#define SPLIT_THRESHOLD 1000
#define MARGIN 1.3

static inline size_t batch_size_from_array(array_t * const ar){
  size_t size = array_size(ar);
  return (size_t) ceil(size / (MARGIN * SPLIT_THRESHOLD));
}

static inline array_t* chunk_from_array(size_t i,
                                        size_t batch_size,
                                        array_t * const ar){
  assert(i < batch_size);

  size_t arr_size = array_size(ar);
  size_t start = i * SPLIT_THRESHOLD;
  size_t end = (i+1 == batch_size) ? arr_size : (i+1) * SPLIT_THRESHOLD;
  pony_ctx_t* ctx = encore_ctx();
  return (array_t* const) array_get_chunk(&ctx, start, end, ar);
}

par_t* party_each(pony_ctx_t **ctx, array_t* const ar){
  pony_type_t* type = array_get_type(ar);
  par_t* root = new_par_empty(ctx, type);

  size_t batch_size = batch_size_from_array(ar);

  for(size_t i=0; i<batch_size; i++){
    array_t * const chunk = chunk_from_array(i, batch_size, ar);
    par_t* par = new_par_array(ctx, chunk, type);
    root = new_par_p(ctx, root, par, type);
  }
  return root;
}

//----------------------------------------
// INTERSECTION COMBINATOR
//----------------------------------------

struct env_collect_from_party {
  closure_t *call;
  closure_t *cmp;
  future_t *promise;
  par_t *par;
  int counter;
};

static void trace_collect_from_party(pony_ctx_t *_ctx, void *p) {
  pony_ctx_t** ctx = &_ctx;
  struct env_collect_from_party *this = p;
  encore_trace_object(*ctx, this->par, party_trace);
  encore_trace_object(*ctx, this->promise, future_trace);
  encore_trace_object(*ctx, this->call, closure_trace);
  encore_trace_object(*ctx, this->cmp, closure_trace);
}

static value_t
grouping_futures_async(pony_ctx_t** ctx,
                       __attribute__ ((unused)) pony_type_t** runtimeType,
                       __attribute__ ((unused)) value_t _args[],
                       void* env) {
  par_t *par = ((struct env_collect_from_party*) env)->par;
  closure_t *clos = ((struct env_collect_from_party*) env)->call;
  closure_t *cmp = ((struct env_collect_from_party*) env)->cmp;
  future_t *promise = ((struct env_collect_from_party*) env)->promise;

  int prev_counter =
      __atomic_fetch_sub(&((struct env_collect_from_party*) env)->counter,
                         1,
                         __ATOMIC_RELAXED);

  if (prev_counter == 1) {
    value_t args[] = { [0] = {.p=par}, [1] = {.p = cmp} };
    value_t result = closure_call(ctx, clos, args);
    future_fulfil(ctx, promise, result);
  }
  return (value_t) {.p = NULL};
}


// OPTIMISATION: it should be done in parallel. Spawn task inside.
// OPTIMISATION: It would be great to have a SetPar struture and perform
//               a transformation from ParT to SetParT, HashParT, etc
static value_t intersection_as_closure(pony_ctx_t** ctx,
                                       pony_type_t** runtimeType,
                                       value_t args[],
                                       __attribute__ ((unused)) void* env)
{
  assert(env == NULL);

  // Initially we got two ParT which were merged into one.
  // Unmerge the ParT and perform their intersection
  par_t *p = args[0].p;
  par_t *left = party_get_parleft(p);
  par_t *right = party_get_parright(p);
  closure_t *cmp = args[1].p;
  pony_type_t *type = runtimeType[0];

  // 1. convert right to Set
  // 2. convert left to Set
  // 3. intersection
  set_s *sl = party_to_set(ctx, left, cmp, type);
  set_s *sr = party_to_set(ctx, right, cmp, type);
  set_s *result = party_set_intersection(ctx, sl, sr);
  return (value_t){.p = party_set_to_party(ctx, result, type) };
}

static value_t distinct_as_closure(pony_ctx_t** ctx,
                                   pony_type_t** runtimeType,
                                   value_t args[],
                                   __attribute__ ((unused)) void* env)
{
  assert(env == NULL);
  par_t *p = args[0].p; // it's always ParT with all values fulfilled!
  closure_t *cmp = args[1].p;
  pony_type_t *type = runtimeType[0];
  set_s *set = party_to_set(ctx, p, cmp, type);
  return (value_t){.p = party_set_to_party(ctx, set, type) };
}

static array_t* collect_future_from_party(pony_ctx_t **ctx, par_t *p)
{
  list_t *l = NULL;
  list_t *futures = NULL;
  size_t counter = 0;
  par_t *current = p;

  while(current){
    PTAG tag = party_tag(current);
    if (tag == EMPTY_PAR) {
      l = list_pop(l, (value_t*)&current);
    } else if (tag == PAR_PAR) {
      par_t *pright = party_get_parright(current);
      par_t *pleft = party_get_parleft(current);
      current = pleft;
      l = list_push(l, (value_t){.p = pright } );
    } else if (tag == FUTURE_PAR) {
      ++counter;
      futures = list_push(futures, (value_t) {.p = party_get_fut(current) });
      l = list_pop(l, (value_t*)&current);
    } else if(tag == FUTUREPAR_PAR){
      ++counter;
      futures = list_push(futures, (value_t) {.p = party_get_futpar(current) });
      l = list_pop(l, (value_t*)&current);
    } else {
      l = list_pop(l, (value_t*)&current);
    }
  }

  array_t *a = array_mk(ctx, counter, &future_type);
  future_t *fut = NULL;
  for (size_t i = 0; i < counter ; ++i){
    futures = list_pop(futures, (value_t*)&fut);
    array_set(a, i, (value_t){.p = fut } );
  }
  return a;
}

// This is a helper function for the common pattern of awaiting until all
// futures have been fulfilled and fulfilling a promise to continue.
// INFO: the promise needs to be fulfilled with a ParT. if this is not the
//       case, the tracing of the promise will fail.
static inline par_t* party_promise_await_on_futures(pony_ctx_t **ctx,
                                                    par_t *par,
                                                    closure_t *call,
                                                    closure_t *cmp){
  array_t *fut_list = collect_future_from_party(ctx, par);
  size_t size = array_size(fut_list);

  if (!size) {
    // runs synchronously. it is up to the call closure to divide and conquer
    // TODO: if there are tasks spawned within the closure, the args array
    //       is allocated on the stack and the arguments may be gone by the time
    //       the tasks start doing any work (no use of task, at the moment)
    value_t args[] = { [0] = { .p = par }, [1] = { .p = cmp }};
    return (par_t *) closure_call(ctx, call, args).p;
  }

  // INFO: make sure the promise is fulfilled with a ParT type.
  future_t *promise = future_mk(ctx, &party_type);

  struct env_collect_from_party *env = encore_alloc(*ctx, sizeof(struct env_collect_from_party));
  env->counter = size;
  env->par = par;
  env->promise = promise;
  env->call = call;
  env->cmp = cmp;

  closure_t *c = closure_mk(ctx, grouping_futures_async,
                            env, trace_collect_from_party, NULL);

  for(size_t i=0; i < size; ++i){
    future_t *future = array_get(fut_list, i).p;
    future_register_callback(ctx, future, c);
  }
  future_await(ctx, promise);
  return (par_t *) future_get_actor(ctx, promise).p;
}

par_t* party_intersection(pony_ctx_t **ctx,
                          par_t *par_left,
                          par_t *par_right,
                          closure_t *cmp,
                          pony_type_t *type){
  par_t *par = new_par_p(ctx, par_left, par_right, type);
  closure_t *call = closure_mk(ctx, intersection_as_closure, NULL, NULL, &type);
  return party_promise_await_on_futures(ctx, par, call, cmp);
}

//----------------------------------------
// DISTINCT COMBINATOR
//----------------------------------------

par_t* party_distinct(pony_ctx_t **ctx,
                      par_t *par,
                      closure_t *cmp,
                      pony_type_t *type){
  closure_t *call = closure_mk(ctx, distinct_as_closure, NULL, NULL, &type);
  return party_promise_await_on_futures(ctx, par, call, cmp);
}


//----------------------------------------
// ZIPWITH COMBINATOR
//----------------------------------------

static list_t* party_leaves_to_list(pony_ctx_t **ctx, par_t * p){
  list_t *list = NULL;
  list_t *tmp_list = NULL;
  while(p){
    switch(p->tag){
    case EMPTY_PAR: {
      tmp_list = list_pop(tmp_list, (value_t*)&p);
      break;
    }
    case VALUE_PAR: {
      list = list_append(list, (value_t) { .p = p });
      tmp_list = list_pop(tmp_list, (value_t*)&p);
      break;
    }
    case FUTURE_PAR: {
      list = list_append(list, (value_t) { .p = p });
      tmp_list = list_pop(tmp_list, (value_t*)&p);
      break;
    }
    case PAR_PAR: {
      par_t *right = p->data.p.right;
      tmp_list = list_push(tmp_list, (value_t) {.p = right });
      p = p->data.p.left;
      break;
    }
    case FUTUREPAR_PAR: {
      future_t *fut = p->data.f.fut;
      future_await(ctx, fut);
      p = future_get_actor(ctx, fut).p;
      break;
    }
    case ARRAY_PAR: {
      array_t* ar = p->data.a.array;
      size_t size = array_size(ar);
      for(size_t i=0; i<size; i++){
        list = list_append(list,
                           (value_t) {.p = new_par_v(ctx,
                                                     array_get(ar, i),
                                                     array_get_type(ar))});
      }
      tmp_list = list_pop(tmp_list, (value_t*)&p);
      break;
    }
    default: exit(-1);
    }
  }
  return list;
}

struct env_zip_future {
  closure_t *cmp_fn;
  par_t *leaf_left;
  par_t *leaf_right;
};

static void trace_zip_future(pony_ctx_t *_ctx, void *p){
  pony_ctx_t** ctx = &_ctx;
  struct env_zip_future *this = p;
  encore_trace_object(*ctx, this->cmp_fn, closure_trace);
  encore_trace_object(*ctx, this->leaf_left, party_trace);
  encore_trace_object(*ctx, this->leaf_right, party_trace);
}

static value_t zip_future_with(pony_ctx_t** ctx,
                               pony_type_t** runtimeType,
                               value_t args[],
                               void* _env){
  value_t v = args[0];

  // Initially right has been set wiht the right ParT.
  // ParT left was a future and now is the current value `v`
  struct env_zip_future* env = _env;
  closure_t *cmpFn = env->cmp_fn;
  par_t *right = env->leaf_right;
  par_t *left = env->leaf_left;
  pony_type_t *result_type = runtimeType[0];

  if (right && !left) {
    // guaranteed to not be PAR_PAR nor FUTUREPAR_PAR nor ARRAY_PAR nor EMPTY
    assert(right->tag != PAR_PAR);
    assert(right->tag != FUTUREPAR_PAR);
    assert(right->tag != ARRAY_PAR);
    assert(right->tag != EMPTY_PAR);
    assert(left == NULL);
    pony_type_t *value_type = runtimeType[1];

    if (right->tag == FUTURE_PAR) {
      // OPTIMISATION: re-use environment
      struct env_zip_future *env_fut = encore_alloc(*ctx, sizeof(struct env_zip_future));
      env_fut->cmp_fn = cmpFn;
      env_fut->leaf_left = new_par_v(ctx, v, value_type);
      closure_t *clos = closure_mk(ctx,
                                   zip_future_with,
                                   env_fut,
                                   trace_zip_future,
                                   runtimeType);

      // this function will return a fut (par t) where the (par t)
      // is guaranteed to be a value.
      future_t *fut = future_chain_actor(ctx, right->data.f.fut, &party_type, clos);
      par_t * futpar = new_par_f(ctx, fut, &party_type);
      return (value_t) { .p = party_join(ctx, futpar) };
    } else {
      assert(right->tag == VALUE_PAR);
      value_t result = closure_call(ctx,
                                    cmpFn,
                                    (value_t[]){v, party_get_v(right)});
      return (value_t) { .p = new_par_v(ctx, result, result_type) };
    }
  } else {
    // We do know that the left Par has already been processed and
    // should have a value
    assert(left->tag == VALUE_PAR);
    value_t result = closure_call(ctx,
                                  cmpFn,
                                  (value_t[]){party_get_v(left), v});
    return (value_t) { .p = new_par_v(ctx, result, result_type) };
  }
}

static inline par_t* party_zip_list(pony_ctx_t **ctx,
                                    list_t *ll,
                                    list_t *lr,
                                    closure_t *fn,
                                    pony_type_t *type){
  par_t *pl = NULL;
  par_t *pr = NULL;
  par_t *result = new_par_empty(ctx, type);
  while(ll){
    ll = list_pop(ll, (value_t*)&pl);
    lr = list_pop(lr, (value_t*)&pr);
    if (pl && pr) {
      if (pl->tag == FUTURE_PAR){
        struct env_zip_future *env = encore_alloc(*ctx, sizeof(struct env_zip_future));
        env->cmp_fn = fn;
        env->leaf_right = pr;

        // TODO: do not allocate things on the stack when
        //       they can be executed by any other actor!
        /* pony_type_t *runtimeTypes[] = { type, future_get_type(pl->data.f.fut) }; */
        pony_type_t **runtimeTypes = encore_alloc(*ctx, 2 * sizeof(*runtimeTypes));
        runtimeTypes[0] = type;
        runtimeTypes[1] = future_get_type(pl->data.f.fut);
        closure_t *clos = closure_mk(ctx,
                                     zip_future_with,
                                     env,
                                     trace_zip_future,
                                     runtimeTypes);

        // this future chanining will return a fut (par t)
        // where the (par t) is guaranteed to be a value.
        future_t *fut = future_chain_actor(ctx, pl->data.f.fut, &party_type, clos);
        par_t * futpar = new_par_f(ctx, fut, &party_type);
        par_t *par = party_join(ctx, futpar);
        result = new_par_p(ctx, result, par, type);
      } else if (pr->tag == FUTURE_PAR) {
        struct env_zip_future *env = encore_alloc(*ctx, sizeof(struct env_zip_future));
        env->cmp_fn = fn;
        env->leaf_left = pl;

        /* pony_type_t *runtimeTypes[] = {type, future_get_type(pl->data.f.fut)}; */
        pony_type_t **runtimeTypes = encore_alloc(*ctx, 2 * sizeof(*runtimeTypes));
        runtimeTypes[0] = type;
        runtimeTypes[1] = future_get_type(pl->data.f.fut);
        closure_t *clos = closure_mk(ctx,
                                     zip_future_with,
                                     env,
                                     trace_zip_future,
                                     runtimeTypes);

        // this future chanining will return a fut (par t)
        // where the (par t) is guaranteed to be a value.
        future_t *fut = future_chain_actor(ctx, pr->data.f.fut, &party_type, clos);
        par_t * futpar = new_par_f(ctx, fut, &party_type);
        par_t *par = party_join(ctx, futpar);
        result = new_par_p(ctx, result, par, type);
      } else {
        // None of them is a future
        assert(pl->tag != FUTURE_PAR);
        assert(pr->tag != FUTURE_PAR);

        value_t args[] = {party_get_v(pl), party_get_v(pr)};
        value_t v = closure_call(ctx, fn, args);
        par_t *par = new_par_v(ctx, v, type);
        result = new_par_p(ctx, result, par, type);
      }
    } else {
      return result;
    }
  }
  return result;
}

par_t* party_zip_with(pony_ctx_t **ctx,
                      par_t *pl,
                      par_t *pr,
                      closure_t *fn,
                      pony_type_t *type){
  list_t *ll = party_leaves_to_list(ctx, pl);
  list_t *lr = party_leaves_to_list(ctx, pr);
  par_t *result = party_zip_list(ctx, ll, lr, fn, type);
  return result;
}
