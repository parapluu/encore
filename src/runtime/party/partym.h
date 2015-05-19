#ifndef PARTYM_H_4815162342
#define PARTYM_H_4815162342

#define new_parT(x) _Generic((x),		\
    int*: new_parV,    	\
    float*: new_parV,	\
    future_s*: new_parF,\
    par_s*: new_par_general,	\
  default: new_parS)((x))
#endif
