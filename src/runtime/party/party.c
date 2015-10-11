#include "party.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "closure.h"

typedef struct fmap_s fmap_s;
typedef par_t* (*fmapfn)(par_t*, fmap_s*);

enum PTAG { EMPTY_PAR, VALUE_PAR, FUTURE_PAR, PAR_PAR, FUTUREPAR_PAR, JOIN_PAR};
enum VAL_OR_PAR {VAL, PAR};

typedef struct EMPTY_PARs {} EMPTY_PARs;
typedef struct VALUE_PARs { value_t val; enum VAL_OR_PAR tag;} VALUE_PARs;
typedef struct FUTURE_PARs { future_t* fut; } FUTURE_PARs;
typedef struct PAR_PARs { struct par_t* left; struct par_t* right; } PAR_PARs;
typedef struct FUTUREPAR_PARs { future_t* fut; } FUTUREPAR_PARs;
typedef struct JOIN_PARs { struct par_t* join; } JOIN_PARs;

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
      struct EMPTY_PARs;
      struct VALUE_PARs;
      struct FUTURE_PARs;
      struct PAR_PARs;
      struct JOIN_PARs;
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
    case JOIN_PAR: {
      party_trace(obj->data.j.join);
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
  pony_type_t const * rtype;
};

/*
 * Parallel constructors
 */

static par_t* init_par(enum PTAG tag, pony_type_t const * const rtype){
  par_t* res = encore_alloc(sizeof* res);
  res->tag = tag;
  res->rtype = rtype;
  return res;
}

par_t* new_par_empty(pony_type_t const * const rtype){
  return init_par(EMPTY_PAR, rtype);
}

par_t* new_par_v(encore_arg_t val, pony_type_t const * const rtype){
  par_t* p = init_par(VALUE_PAR, rtype);
  p->data.val = val;
  p->data.tag = VAL;
  return p;
}

par_t* new_par_f(future_t* const f, pony_type_t const * const rtype){
  par_t* p = init_par(FUTURE_PAR, rtype);
  p->data.fut = f;
  return p;
}

par_t* new_par_p(par_t* const p1, par_t* const p2, pony_type_t const * const rtype){
  par_t* p = init_par(PAR_PAR, rtype);
  p->data.left = p1;
  p->data.right = p2;
  return p;
}

par_t* new_par_fp(future_t* const f, pony_type_t const * const rtype){
  par_t* p = init_par(FUTUREPAR_PAR, rtype);
  p->data.fut = f;
  return p;
}

par_t* new_par_join(par_t* const p, pony_type_t const * const rtype){
  par_t* par = init_par(JOIN_PAR, rtype);
  par->data.join = p;
  return par;
}

//---------------------------------------
// SEQUENCE COMBINATOR
//---------------------------------------

// Forward declaration
static par_t* fmap(closure_t* const f, par_t* const in, pony_type_t const * const rtype);

static value_t fmap_party_closure(value_t args[], void* const env){
  par_t* p = (par_t*)args[0].p;
  fmap_s* fm = env;
  return (value_t){.p = fmap(fm->fn, p, get_rtype(fm))};
}

static inline future_t* chain_party_to_function(par_t* const in, fmap_s* const fm, closure_fun clos){
  future_t* rf = (future_t*)future_mk(in->rtype);
  closure_t* cp = closure_mk(clos, fm, NULL);
  return future_chain_actor(in->data.fut, (future_t*)rf, cp);
}

static inline par_t* fmap_run_v(par_t* const in, fmap_s* const f){
  value_t v = closure_call(f->fn, (value_t[]){in->data.val});
  return new_par_v(v, get_rtype(f));
}

static inline par_t* fmap_run_f(par_t* const in, fmap_s* const f){
  future_t* rf = (future_t*)future_mk(in->rtype);
  future_t* chained_fut = future_chain_actor(in->data.fut, rf, f->fn);
  return new_par_f(chained_fut, get_rtype(f));
}

static inline par_t* fmap_run_fp(par_t* const in, fmap_s* const f){
  future_t* fut = chain_party_to_function(in, f, fmap_party_closure);
  return new_par_fp(fut, get_rtype(f));
}

// WARNING:
// this function is only used for fmap J, which guarantees
// that args is of: {.p = par_t* }
static encore_arg_t fmap_fmap_closure(value_t args[], void* const env){
  fmap_s* fm = env;
  encore_arg_t arg = args[0];
  return (encore_arg_t) {.p = fmap(fm->fn, (par_t*) arg.p, fm->rtype)};
}

static inline par_t* fmap_run_j(par_t* const in, fmap_s* const f){
  fmap_s* fm = (fmap_s*)encore_alloc(sizeof* fm);
  *fm = (fmap_s){.fn=f->fn, .rtype = f->rtype};
  closure_t* clos = closure_mk(fmap_fmap_closure, fm, NULL);
  par_t* p = fmap(clos, in->data.join, get_rtype(fm));
  return new_par_join(p, get_rtype(f));
}

/**
 *  fmap: (a -> b) -> Par a -> Par b
 *
 *  @param f Closure function to be called
 *  @param in Parallel collection data structure to call \p f on
 *  @param rtype The returned runtime type (ENCORE_PRIMITIVE or ENCORE_ACTIVE)
 *  @return a pointer to a new parallel collection of \p rtype runtime type
 */

static par_t* fmap(closure_t* const f, par_t* const in,
                   pony_type_t const * const rtype){
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
  case JOIN_PAR: return fmap_run_j(in, fm);
  default: assert(0); exit(-1);
  }
}


par_t* party_sequence(par_t* const p, closure_t* const f,
                      pony_type_t const * const rtype){
  return fmap(f, p, rtype);
}

//---------------------------------------
// JOIN COMBINATOR
//---------------------------------------

static inline par_t* party_join_v(par_t* const p){
   return ((par_t*)p->data.val.p);
}

static inline par_t* party_join_p(par_t* const p){
  if(p->data.left->tag == EMPTY_PAR){
    return party_join(p->data.right);
  }else if(p->data.right->tag == EMPTY_PAR){
    return party_join(p->data.left);
  }else{
    par_t* pleft = party_join(p->data.left);
    par_t* pright = party_join(p->data.right);
    assert(get_rtype(pleft) == get_rtype(pright));
    return new_par_p(pleft, pright, get_rtype(pleft));
  }
}

static value_t party_join_fp_closure(value_t args[],
                                     void* __attribute__ ((unused)) env){
  par_t* const p = (par_t*)args[0].p;
  return (value_t){.p = party_join(p)};
}

static inline par_t* party_join_fp(par_t* const p){
  future_t* const fut = p->data.fut;
  future_t* const chained_fut = future_mk(p->rtype);
  closure_t* const clos = closure_mk(party_join_fp_closure, NULL, NULL);
  future_chain_actor(fut, chained_fut, clos);
  return new_par_fp(chained_fut, p->rtype);
}

par_t* party_join(par_t* const p){
  switch(p->tag){
  case EMPTY_PAR: return p;
  case VALUE_PAR: return party_join_v(p);
  case FUTURE_PAR: return new_par_fp(p->data.fut, get_rtype(p));
  case PAR_PAR: return party_join_p(p);
  case FUTUREPAR_PAR: return party_join_fp(p);
  case JOIN_PAR: return party_join(party_join(p->data.join));
  default: assert_error(); return NULL;
  }
}
