#ifndef PARTY_H_412139
#define PARTY_H_412139

#include "partym.h"

typedef struct future_s future_s;
typedef struct par_s par_s;
typedef struct list_s list_s;
typedef par_s* (*prune_fn)(void* f);
typedef par_s* (*select_fn)(par_s* p);

// -------------------------------------------
// Constructors
// -------------------------------------------

par_s* new_par_empty();
#define new_par(x) new_parT(x)
par_s* new_par_p(par_s* p1, par_s* p2);

// -------------------------------------------
// Combinators
// -------------------------------------------

// sequence :: Par t -> (t -> t') -> Par t'
par_s* sequence(par_s* p);

// pjoin :: Par (Par a) -> Par a
par_s* pjoin(par_s* p);

// extract :: Par t -> [t]
list_s extract(par_s* t);

// prune :: (Fut (Maybe t) -> Par t') -> Par t -> Par t'
par_s* prune(prune_fn f, par_s* p);

// otherwise :: Par t -> Delay (Par t) -> Par t
par_s* otherwise(par_s* p, par_s* delay);

// select :: (Maybe t -> Par t') -> Par t -> Par t'
par_s* select(select_fn f, par_s* p);

// peek :: Par t -> Fut (Maybe t)
future_s* peek(par_s* p);

// each :: [t] -> Par t
par_s* each(list_s* l);

#endif
