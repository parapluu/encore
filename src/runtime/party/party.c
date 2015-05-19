#include <stdlib.h>
#include "party.h"
#include <assert.h>

/*
data Par a where
  S :: Par a
  V :: a -> Par a
  F :: Fut a -> Par a
  P :: Par a -> Par a -> Par a
  J :: Par (Par a) -> Par a
  FP :: Fut (Par a) -> Par a
  M  :: Int -> Int -> Array a -> Par a

*/


typedef void *future_t;

/* typedef void *future_s; */

typedef void *function;

box apply(function f, box v) {
    /* magic */
    return NULL;
}

future_t chain(future_t fut, function f) {
    /* magic */
    return NULL;
}


#define DECLARE_PAR(name, ...) typedef struct name##s __VA_ARGS__ name##s;

DECLARE_PAR(S, {});
DECLARE_PAR(V, { box val; });
DECLARE_PAR(F, { future_t fut; });
DECLARE_PAR(P, { struct par_s* left; struct par_s* right; });
DECLARE_PAR(J, { struct par_s* /* Par */ join; });
DECLARE_PAR(M, { int size; int used; box *data; });
DECLARE_PAR(FP, { future_t fut; });

enum PTAG { S, V, F, P, J, FP, M };  // be less concise
struct par_s {
    enum PTAG tag;
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


#define INIT_PAR(type, ...){ \
par_s* res = malloc(sizeof(par_s));	\
    res->tag = type; \
    __VA_ARGS__; \
    return res; \
}\

par_s* new_parS(void){
  INIT_PAR(S);
}

par_s* new_parV(int* val){
  INIT_PAR(V, res->data.val = val);
}

par_s* new_parF(future_t* f){
  INIT_PAR(F, res->data.fut = f)
}

par_s* new_parP(par_s* p1, par_s* p2){
  INIT_PAR(P, res->data.left = p1, res->data.right = p2;)
}

par_s* new_par_general(par_s* p){
  switch(p->tag){
  case J:
    INIT_PAR(J, res->data.join = p);
    break;
  case FP:
    assert(0);
    exit(-1);
    // break;
  case M:
    assert(0);
    exit(-1);
  default:
    // other cases should not be directed to this function
    assert(0);
    exit(-1);
  }
}


/* Par fmap(Par in, function f) { */
/*     switch (in->tag) { */
/*         case S: */
/*         return newS(); */
/*         break; */

/*         case V: */
/*         return newV(apply(f,in->data.v.val)); */
/*         break; */

/*         case F: */
/*         return newF(chain(in->data.f.fut,f)); */
/*         break; */

/*         case P: */
/*         return newP(fmap(in->data.p.left, f), fmap(in->data.p.left, f)); */
/*         break; */

/*         case J: */
/*         // not sure what to do. need to build function that will call fmap(f) */
/*         // and pass that in */
/*         break; */

/*         case FP: */
/*         // not sure what to do. need to build function that will call fmap(f) */
/*         // and pass that in */
/*         break; */

/*         case M: */
/*         // TODO: do */
/*         break; */
/*     } */
/*     return NULL; */
/* } */


int main() {
  int* i = malloc(sizeof(int));
  par_s* x = (par_s*) malloc(sizeof(par_s));
  *i = 42;
  par_s* p = new_par(i);
  new_par(x);
  new_par_empty();

  return 0;
}
