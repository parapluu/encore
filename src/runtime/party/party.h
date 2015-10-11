#ifndef PARTY_H_412139
#define PARTY_H_412139

#include "encore.h"

typedef struct par_t par_t;
extern pony_type_t party_type;

void party_trace(void*);

par_t* new_par_empty(pony_type_t const * const rtype);
par_t* new_par_v(encore_arg_t val, pony_type_t const * const rtype);
par_t* new_par_f(future_t* const fut, pony_type_t const * const rtype);
par_t* new_par_p(par_t* const p1, par_t* const p2, pony_type_t const * const rtype);
par_t* new_par_fp(future_t* const f, pony_type_t const * const rtype);
par_t* new_par_join(par_t* const p, pony_type_t const * const rtype);

/**
 *  sequence :: Par t -> (t -> t') -> Par t'
 *
 *  Given a Par t and a function, execute the function for every Par t element
 *
 *  @param p Parallel collection
 *  @param closure The closure to be called
 *  @return The pointer to the parallel collection
 */

par_t* party_sequence(par_t* const p, closure_t* const closure,
                      pony_type_t const * const rtype);

/**
 * join :: Par (Par t) -> Par t
 * @param p Parallel collection with type Par (Par t)
 * @return Pointer to a new parallel collection of type Par t
 */

par_t* party_join(par_t* const p);

#endif
