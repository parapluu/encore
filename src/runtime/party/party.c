#include <stdlib.h>
#include "party.h"

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


enum PTAG { S, V, F, P, J, FP, M };  // be less concise

typedef struct Par_s *Par;  // forward declaration

typedef struct {} Ss;

typedef struct Vs { value_t val; } Vs;

typedef struct { future_t* fut; } Fs;

typedef struct { Par left; Par right; } Ps;

typedef struct{ Par /* Par */ join; } Js;

typedef struct { int size; int used; value_t *data; /* array */ } Ms;

typedef struct { future_t* fut; } FPs;

struct Par_s {
    enum PTAG tag;
    union ParU {
        Ss s;
        Vs v;
        Fs f;
        Ps p;
        Js j;
        FPs fp;
        Ms m;
    } data;   
};

Par newS() {
    Par res = (Par)malloc(sizeof (struct Par_s));
    res->tag = S;
    return res;
}

Par newV(value_t val) {
    Par res = (Par)malloc(sizeof (struct Par_s));
    res->tag = V;
    res->data.v.val = val;
    return res;
}

Par newF(future_t *fut) {
    Par res = (Par)malloc(sizeof (struct Par_s));
    res->tag = F;
    res->data.f.fut = fut;
    return res;
}

Par newP(Par p1, Par p2) {
    Par res = (Par)malloc(sizeof (struct Par_s));
    res->tag = P;
    res->data.p.left  = p1;
    res->data.p.right = p2;
    return res;
}

Par newJ(Par p) {
    Par res = (Par)malloc(sizeof (struct Par_s));
    res->tag = J;
    res->data.j.join  = p;
    return res;
}

Par fmap(Par in, closure_t *f) {
    switch (in->tag) {
        case S:
        return newS();
        break;
        
        case V:
        return newV(closure_call(f,&(in->data.v.val)));
        break; 
        
        case F: {
            future_t *rfut = future_mk(NULL);  /* NEEDS pony_type */ 
            return newF(future_chain_actor(in->data.f.fut, rfut, f));
        }
        break; 
        
        case P:
        return newP(fmap(in->data.p.left, f), fmap(in->data.p.right, f));
        break;
        
        case J:
        // not sure what to do. need to build function that will call fmap(f)
        // and pass that in
        break;
        
        case FP: 
        // not sure what to do. need to build function that will call fmap(f)
        // and pass that in
        break;
        
        case M:
        // TODO: do
        break;
    }
    return NULL; 
}

int main() {
    return 0;
}
