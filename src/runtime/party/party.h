#ifndef PARTY_H_412139
#define PARTY_H_412139

#include "encore.h"
#include <array.h>

typedef struct par_t par_t;
extern pony_type_t party_type;

void party_trace(pony_ctx_t*, void*);

par_t* new_par_empty(pony_ctx_t **ctx, pony_type_t const * const rtype);
par_t* new_par_v(pony_ctx_t **ctx, encore_arg_t val, pony_type_t const * const rtype);
par_t* new_par_f(pony_ctx_t **ctx,future_t* const fut, pony_type_t const * const rtype);
par_t* new_par_p(pony_ctx_t **ctx, par_t* const p1, par_t* const p2, pony_type_t const * const rtype);
par_t* new_par_fp(pony_ctx_t **ctx, future_t* const f, pony_type_t const * const rtype);
par_t* new_par_array(pony_ctx_t **ctx, array_t* arr, pony_type_t const * const rtype);

/* par_t* new_par_join(par_t* const p, pony_type_t const * const rtype); */

/**
 *  sequence :: Par t -> (t -> t') -> Par t'
 *
 *  Given a Par t and a function, execute the function for every Par t element
 *
 *  @param p Parallel collection
 *  @param closure The closure to be called
 *  @return The pointer to the parallel collection
 */

par_t* party_sequence(pony_ctx_t **ctx, par_t* p, closure_t* const closure,
                      pony_type_t const * const rtype);

/**
 * join :: Par (Par t) -> Par t
 * @param p Parallel collection with type Par (Par t)
 * @return Pointer to a new parallel collection of type Par t
 */

par_t* party_join(pony_ctx_t **ctx, par_t* const p);

/**
 * extract :: Par t -> [t]
 *
 * Given a Par t, return an array of the computed elements in the collection
 *
 * @param p Parallel collection
 * @param type Runtime type
 * @return Pointer to an array
 */

array_t* party_extract(pony_ctx_t **ctx, par_t* p, pony_type_t *type);

/**
 * each :: [t] -> Par t
 *
 * Given an array of type t, return a Par t
 *
 * @param array Array to convert to a parallel collection
 * @return Parallel collection
 */
par_t* party_each(pony_ctx_t **ctx, array_t * const array);

#endif
