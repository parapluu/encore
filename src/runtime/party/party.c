#include "party.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "closure.h"

extern pony_type_t* encore_task_type;
typedef struct fmap_s fmap_s;
typedef par_t* (*fmapfn)(par_t*, fmap_s*);

enum PTAG { EMPTY_PAR, VALUE_PAR, FUTURE_PAR, PAR_PAR, FUTUREPAR_PAR};
enum VAL_OR_PAR {VAL, PAR};

typedef struct EMPTY_PARs {} EMPTY_PARs;
typedef struct VALUE_PARs { value_t val; enum VAL_OR_PAR tag;} VALUE_PARs;
typedef struct FUTURE_PARs { future_t* fut; } FUTURE_PARs;
typedef struct PAR_PARs { struct par_t* left; struct par_t* right; } PAR_PARs;
typedef struct FUTUREPAR_PARs { future_t* fut; } FUTUREPAR_PARs;

struct par_t {
    enum PTAG tag;
    pony_type_t* rtype;
    union ParU {
      EMPTY_PARs s;
      VALUE_PARs v;
      FUTURE_PARs f;
      PAR_PARs p;
      FUTUREPAR_PARs fp;
      struct EMPTY_PARs;
      struct VALUE_PARs;
      struct FUTURE_PARs;
      struct PAR_PARs;
    } data;
};

pony_type_t party_type =
  {
    .id=ID_PARTY,
    .size=sizeof(struct par_t),
    .trace=party_trace
  };

#define get_rtype(x) (x)->rtype

__attribute__ ((noreturn))
static inline void assert_error(){
  assert(0);
  exit(-1);
}

void party_trace(void* p){
  par_t *obj = p;
  if(obj->rtype == ENCORE_ACTIVE){
    pony_traceactor((pony_actor_t*) obj->data.v.val.p);
  }else if(obj->rtype != ENCORE_PRIMITIVE){
    switch(obj->tag){
    case EMPTY_PAR: break;
    case VALUE_PAR: {
      pony_traceobject(obj->data.v.val.p, obj->rtype->trace);
      break;
    }
    case FUTURE_PAR: {
      pony_traceobject(obj->data.f.fut, obj->rtype->trace);
      break;
    }
    case PAR_PAR: {
      party_trace(obj->data.p.left);
      party_trace(obj->data.p.right);
      break;
    }
    case FUTUREPAR_PAR: {
      pony_traceobject(obj->data.fp.fut, obj->rtype->trace);
      break;
    }
    default: {
      assert(0);
      exit(-1);
    }
    }
  }
}

struct fmap_s{
  closure_t* fn;
  pony_type_t* rtype;
};

/*
 * Parallel constructors
 */

static par_t* init_par(enum PTAG tag, pony_type_t* rtype){
  par_t* res = encore_alloc(sizeof* res);
  res->tag = tag;
  res->rtype = rtype;
  return res;
}

par_t* new_par_empty(pony_type_t* rtype){
  return init_par(EMPTY_PAR, rtype);
}

par_t* new_par_v(encore_arg_t val, pony_type_t* rtype){
  par_t* p = init_par(VALUE_PAR, rtype);
  p->data.val = val;
  p->data.tag = VAL;
  return p;
}

par_t* new_par_f(future_t* f, pony_type_t* rtype){
  par_t* p = init_par(FUTURE_PAR, rtype);
  p->data.fut = f;
  return p;
}

par_t* new_par_p(par_t* p1, par_t* p2, pony_type_t* rtype){
  par_t* p = init_par(PAR_PAR, rtype);
  p->data.left = p1;
  p->data.right = p2;
  return p;
}

par_t* new_par_fp(future_t* f, pony_type_t* rtype){
  par_t* p = init_par(FUTUREPAR_PAR, rtype);
  p->data.fut = f;
  return p;
}

// Forward declaration
static par_t* fmap(closure_t* f, par_t* in, pony_type_t* rtype);

static value_t fmap_party_closure(value_t args[], void* env){
  par_t* p = (par_t*)args[0].p;
  fmap_s* fm = env;
  return (value_t){.p = fmap(fm->fn, p, get_rtype(fm))};
}

static value_t fmap_party_v_closure(value_t args[], void* env){
  fmap_s* fm = env;
  encore_arg_t e = args[0];
  return closure_call(fm->fn, &e);
}

static inline future_t* chain_party_to_function(par_t* in, fmap_s* fm, closure_fun clos){
  future_t* rf = (future_t*)future_mk(encore_task_type);
  closure_t* cp = closure_mk(clos, fm, NULL);
  return (future_t*)future_chain_actor((future_t*)in->data.fut, (future_t*)rf, cp);
}

static inline par_t* fmap_run_v(par_t* in, fmap_s* f){
  value_t v = closure_call(f->fn, (value_t[]){in->data.val});
  return new_par_v(v, get_rtype(f));
}

static inline par_t* fmap_run_f(par_t* in, fmap_s* f){
  future_t* p = chain_party_to_function(in, f, fmap_party_v_closure);
  return new_par_f(p, get_rtype(f));
}

static inline par_t* fmap_run_fp(par_t* in, fmap_s* f){
  future_t* fut = chain_party_to_function(in, f, fmap_party_closure);
  return new_par_fp(fut, get_rtype(f));
}

/**
 *  fmap: (a -> b) -> Par a -> Par b
 *
 *  @param f Closure function to be called
 *  @param in Parallel collection data structure to call \p f on
 *  @param rtype The returned runtime type (ENCORE_PRIMITIVE or ENCORE_ACTIVE)
 *  @return a pointer to a new parallel collection of \p rtype runtime type
 */

static par_t* fmap(closure_t* f, par_t* in, pony_type_t* rtype){
  fmap_s *fm = (fmap_s*) encore_alloc(sizeof* fm);
  *fm = (fmap_s){.fn = f, .rtype=rtype};
  switch(in->tag){
  case EMPTY_PAR: return new_par_empty(rtype);
  case VALUE_PAR: return fmap_run_v(in, fm);
  case FUTURE_PAR: return fmap_run_f(in, fm);
  case PAR_PAR: {
    par_t* left = fmap(f, in->data.left, rtype);
    par_t* right = fmap(f, in->data.right, rtype);
    return new_par_p(left, right, rtype);
  }
  case FUTUREPAR_PAR: return fmap_run_fp(in, fm);
  default: assert(0); exit(-1);
  }
}

par_t* party_sequence(par_t* p, closure_t* f, pony_type_t* rtype){
  return fmap(f, p, rtype);
}
