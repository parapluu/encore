#ifndef PARTY_STRUCTURE
#define PARTY_STRUCTURE

#include "party.h"

typedef enum PTAG {
  EMPTY_PAR,
  VALUE_PAR,
  FUTURE_PAR,
  FUTUREPAR_PAR,
  PAR_PAR,
  ARRAY_PAR
} PTAG;


PTAG party_tag(par_t const * const p);

encore_arg_t party_get_v(par_t const * const p);

future_t* party_get_fut(par_t const * const p);

future_t* party_get_futpar(par_t const * const p);

par_t* party_get_parleft(par_t const * const p);

par_t* party_get_parright(par_t const * const p);

#endif
