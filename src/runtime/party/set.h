#ifndef SET_H_2017_02_08
#define SET_H_2017_02_08
#include <encore.h>
#include "party.h"

typedef struct set_s set_s;

set_s* party_new_set(pony_ctx_t **ctx, closure_t *cmp);

set_s* party_set_add(pony_ctx_t **ctx, set_s* s, value_t data);

par_t* party_set_to_party(pony_ctx_t **ctx, set_s* s, pony_type_t *type);

bool party_set_lookup(pony_ctx_t **ctx, set_s *s, value_t data);

set_s* party_set_intersection(pony_ctx_t **ctx, set_s* sl, set_s* sr);

set_s* party_to_set(pony_ctx_t **ctx,
                    par_t *p,
                    closure_t *cmp,
                    pony_type_t *type);

#endif
