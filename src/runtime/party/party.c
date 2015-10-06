#include "party.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "future.h"
#include "closure.h"
#include "array.h"
#include "task.h"

#ifndef NDEBUG
#define encore_alloc malloc
#endif

extern pony_type_t* encore_task_type;
typedef struct fmap_s fmap_s;
typedef par_t* (*fmapfn)(par_t*, fmap_s*);
enum PTAG { S, V, F, P, J, FP, M };  // be less concise
enum V_OR_P {VAL, PAR}; // represents if content in val is value or pointer to par_t

#define DECLARE_PAR(name, ...) typedef struct name##s __VA_ARGS__ name##s;
DECLARE_PAR(S, {});
DECLARE_PAR(V, {
    value_t val;
    enum V_OR_P tag;
});
DECLARE_PAR(F, { future_s fut; });
DECLARE_PAR(P, { struct par_t* left; struct par_t* right; });
DECLARE_PAR(J, { struct par_t* /* Par */ join; });
DECLARE_PAR(M, { int size; int used; value_t *l; }); //review
DECLARE_PAR(FP, { future_s fut; });

struct par_t {
    enum PTAG tag;
    pony_type_t* rtype;
    struct par_t* p;
    union ParU {
      Ss s;
      Vs v;
      Fs f;
      Ps p;
      Js j;
      FPs fp;
      Ms m;
      struct Ss;
      struct Vs;
      struct Fs;
      struct Ps;
      struct Js;
      /* struct FPs; */ // redeclares Fs (identical struct)
      struct Ms;
    } data;
};


pony_type_t party_type =
  {
    .id=ID_PARTY,
    .size=sizeof(struct par_t),
    .trace=party_trace
  };

#define get_rtype(x) (x)->rtype

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
    case S: break;
    case V: {
      pony_traceobject(obj->data.v.val.p, obj->rtype->trace);
      break;
    }
    case F: {
      pony_traceobject(obj->data.f.fut, obj->rtype->trace);
      break;
    }
    case P: {
      party_trace(obj->data.p.left);
      party_trace(obj->data.p.right);
      break;
    }
    case J: {
      party_trace(obj->data.j.join);
      break;
    }
    case FP: {
      pony_traceobject(obj->data.fp.fut, obj->rtype->trace);
      break;
    }
    case M: {
      // TODO: GC M
      assert(0);
      exit(-1);
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

// Forward declaration of function
par_t* party_sequence(par_t* p, closure_t* f, pony_type_t* rtype);
par_t* party_pjoin(par_t* p);
array_t* party_extract(par_t* t);
par_t* party_prune(closure_t* f, par_t* p);
par_t* party_otherwise(par_t* p, par_t* delay);
par_t* party_select(closure_fun f, par_t* p);
future_t* party_peek(par_t* p);
par_t* party_each(array_t* l);


//-----------------------------------


/*
 * Parallel constructors
 */

static inline par_t* fmap_run_v(par_t* in, fmap_s* f);
static inline par_t* fmap_run_j(par_t* in, fmap_s* f);
static inline par_t* fmap_run_f(par_t* in, fmap_s* f);
static inline par_t* fmap_run_m(par_t* in, fmap_s* f);
static inline par_t* fmap_run_s(par_t* in, fmap_s* f);
static inline par_t* fmap_run_p(par_t* in, fmap_s* f);
static inline par_t* fmap_run_fp(par_t* in, fmap_s* f);

static par_t* init_par(enum PTAG tag, pony_type_t* rtype){
  par_t* res = encore_alloc(sizeof* res);
  res->p = NULL;
  res->tag = tag;
  res->rtype = rtype;
  return res;
}

par_t* new_par_empty(pony_type_t* rtype){
  return init_par(S, rtype);
}

par_t* new_par_v(encore_arg_t val, pony_type_t* rtype){
  par_t* p = init_par(V, rtype);
  p->data.val = val;
  p->data.tag = VAL;
  return p;
}

par_t* new_par_f(future_s f, pony_type_t* rtype){
  par_t* p = init_par(F, rtype);
  p->data.fut = f;
  return p;
}

par_t* new_par_p(par_t* p1, par_t* p2, pony_type_t* rtype){
  par_t* p = init_par(P, rtype);
  p->data.left = p1;
  p->data.right = p2;
  return p;
}

par_t* new_par_fp(future_s f, pony_type_t* rtype){
  par_t* p = init_par(FP, rtype);
  p->data.fut = f;
  return p;
}

par_t* new_par_join(par_t* p, pony_type_t* rtype){
  par_t* par = init_par(J, rtype);
  par->data.join = p;
  return par;
}

/* // TODO: not a list, an array -> we don't care about `used` */
/* par_t* new_par_m(int size, int used, value_t* l, pony_type_t* rtype){ */
/*   par_t* p = init_par(M, rtype); */
/*   p->data.m = (struct Ms){.size=size, .used=used, .l=l}; */
/*   return p; */
/* } */

par_t* new_par_general(par_t* p, pony_type_t* rtype){
  switch(p->tag){
  case S: return p;
  case V: return new_par_v((encore_arg_t){.p = p}, rtype);
  case J: {
    par_t* new_p = init_par(J, rtype);
    new_p->data.join = p;
    return new_p;
  }
  default: assert_error(); return NULL;
  }
}


/*
 * fmap :: (t -> t') -> Par t -> Par t'
 */

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

static inline future_s chain_party_to_function(par_t* in, fmap_s* fm, closure_fun clos){
  // TODO: why encore_task_type?
  future_s rf = (future_s)future_mk(encore_task_type);
  closure_t* cp = closure_mk(clos, fm, NULL);
  return (future_s)future_chain_actor((future_t*)in->data.fut, (future_t*)rf, cp);
}

static inline par_t* fmap_run_v(par_t* in, fmap_s* f){
  value_t v = closure_call(f->fn, (value_t[]){in->data.val});
  return new_par_v(v, get_rtype(f));
}

// WARNING:
// this function is only used for fmap J, which guarantees
// that args is of: {.p = par_t* }
static encore_arg_t fmap_fmap_closure(value_t args[], void* env){
  fmap_s* fm = env;
  encore_arg_t arg = args[0];
  return (encore_arg_t) {.p = fmap(fm->fn, (par_t*) arg.p, fm->rtype)};
}

static inline par_t* fmap_run_j(par_t* in, fmap_s* f){
  fmap_s* fm = (fmap_s*)encore_alloc(sizeof* fm);
  *fm = (fmap_s){.fn=f->fn, .rtype = f->rtype};
  closure_t* clos = closure_mk(fmap_fmap_closure, fm, NULL);
  par_t* p = fmap(clos, in->data.join, get_rtype(fm));
  return new_par_join(p, get_rtype(f));
}

static inline par_t* fmap_run_f(par_t* in, fmap_s* f){
  future_s p = chain_party_to_function(in, f, fmap_party_v_closure);
  return new_par_f(p, get_rtype(f)); // TODO: not tested
}

// TODO: treat this as an array, not as a list
/* static inline par_t* fmap_run_m(par_t* in, fmap_s* f){ */
/*   assert(in->tag==M); */
/*   array_t* l = map(f->fn, in->data.m.l, get_rtype(f)); */
/*   return new_par_m(in->data.size, in->data.used, l, get_rtype(f)); */
/* } */

static inline par_t* fmap_run_fp(par_t* in, fmap_s* f){
  future_s fut = chain_party_to_function(in, f, fmap_party_closure);
  return new_par_fp(fut, get_rtype(f));
}

/**
 *  fmap: (a -> b) -> Par a -> Par b
 *
 *  @param closure containing function to run
 *  @param parallel type structure
 *  @param the returned runtime type (ENCORE_PRIMITIVE or ENCORE_ACTIVE)
 *  @return a new Par of rtype
 */

static par_t* fmap(closure_t* f, par_t* in, pony_type_t* rtype){
  fmap_s *fm = (fmap_s*) encore_alloc(sizeof* fm);
  *fm = (fmap_s){.fn = f, .rtype=rtype};
  switch(in->tag){
  case S: return new_par_empty(rtype);
  case V: return fmap_run_v(in, fm);
  case F: return fmap_run_f(in, fm);
  case P: {
    return new_par_p(fmap(f, in->data.left, rtype),
		     fmap(f, in->data.right, rtype),
		     rtype);
  }
  case J: return fmap_run_j(in, fm);
  case FP: return fmap_run_fp(in, fm);
  /* case M: return fmap_run_m(in, fm); */
  default: assert(0); exit(-1);
  }
}



static future_t* party_peek_function_application_to_par(par_t* p, closure_fun fun);


/*
 * sequence :: Par t -> (t -> t') -> Par t'
 */

par_t* party_sequence(par_t* p, closure_t* f, pony_type_t* rtype){
  return fmap(f, p, rtype);
}

/*
 * pjoin :: Par (Par a) -> Par a
 */

par_t* party_pjoin(par_t* p){
  assert(0);
  return NULL;
}

/*
 * extract :: Par t -> [t]
 */
array_t* party_extract(par_t* p){
  assert(0);
  return NULL;
}

/*
 * prune :: (Fut (Maybe t) -> Par t') -> Par t -> Par t'
 */
par_t* party_prune(closure_t* fn, par_t* p){
  assert(0);
  return NULL;
}


/*
 * otherwise :: Par t -> Delay (Par t) -> Par t
 */
par_t* party_otherwise(par_t* p, par_t* delay){
  assert(0);
  return NULL;
}

/*
 * select :: (Maybe t -> Par t') -> Par t -> Par t'
 */
par_t* party_select(closure_fun f, par_t* p){
  assert(0);
  return NULL;
}

/*
 * peek :: Par t -> Fut (Maybe t)
 */
future_t* party_peek(par_t* p){
  assert(0);
  return NULL;
}

/*
 * each :: [t] -> Par t
 */
par_t* party_each(array_t* l){
  assert(0);
  exit(-1);
}
