#ifndef PARTYM_H_4815162342
#define PARTYM_H_4815162342

#define new_par_empty new_parS
#define new_par_p new_parP

#define new_parT(x) _Generic((x),		\
    int*: new_parV,    	                        \
    float*: new_parV,	                        \
    void*: new_parV,                            \
    future_s*: new_parF,                        \
    par_s*: new_par_general)((x))
#endif
