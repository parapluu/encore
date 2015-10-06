#ifndef PARTY_H_412139
#define PARTY_H_412139

#include "encore.h"
#include <array.h>

typedef struct par_t par_t;
typedef struct future* future_s;
pony_type_t party_type;

extern par_t* new_par_general(par_t* p, pony_type_t*);
#define new_parT(x, rtype) _Generic((x),	\
    encore_arg_t: new_par_v,   	                \
    future_s: new_par_f,                        \
    par_t*: new_par_general)(x, rtype)

typedef void maybe;
typedef maybe* (*maybe_fn)(maybe*);

void party_trace(void*);

// -------------------------------------------
// Constructors
// -------------------------------------------

#define new_par(x, rtype) new_parT(x, rtype)
par_t* new_par_empty(pony_type_t* rtype);
par_t* new_par_v(encore_arg_t val, pony_type_t* rtype);
par_t* new_par_f(future_s fut, pony_type_t* rtype);
par_t* new_par_p(par_t* p1, par_t* p2, pony_type_t* rtype);
par_t* new_par_fp(future_s f, pony_type_t* rtype);
par_t* new_par_join(par_t* p, pony_type_t* rtype);
par_t* new_par_m(int size, int used, value_t* l, pony_type_t* rtype);

// -------------------------------------------
// Combinators
// -------------------------------------------

/**
 *  sequence :: Par t -> (t -> t') -> Par t'
 *
 *  Given a Par t and a function, execute the function for every Par t element
 *
 *  @param Par t
 *  @param function to execute
 *  @return Par t'
 */

par_t* party_sequence(par_t* p, closure_t* f, pony_type_t* rtype);


/**
 *  join :: Par (Par a) -> Par a
 *
 *  Join a Par Par t into a single Par t
 *
 *  @param par type
 *  @return new par type
 */

par_t* party_pjoin(par_t* p);

/**
 *  extract :: Par t -> [t]
 *
 *  Move from the parallel world to the sequential world
 *
 *  @param par type
 *  @return array of t data type items
 */

array_t* party_extract(par_t* t);

// prune :: (Fut (Maybe t) -> Par t') -> Par t -> Par t'
par_t* party_prune(closure_t* f, par_t* p);

// otherwise :: Par t -> Delay (Par t) -> Par t
par_t* party_otherwise(par_t* p, par_t* delay);


/**
 *  select :: (Maybe t -> Par t') -> Par t -> Par t'
 *
 *  @param closure fun (global function)
 *  @param par t
 *  @return par s containing a future
 */

par_t* party_select(closure_fun f, par_t* p);

/**
 *  peek :: Par t -> Fut (Maybe t)
 *
 *  returns a future to the first computation that finishes in the par t.
 *  in order to get this result, you need to `get` on the future.
 *
 *  @param par t
 *  @return future
 */

future_t* party_peek(par_t* p);

// each :: [t] -> Par t
par_t* party_each(array_t* l);

#endif
