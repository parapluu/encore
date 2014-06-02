#ifndef mpmcq_h
#define mpmcq_h

#include <stdint.h>

typedef struct mpmcq_node_t mpmcq_node_t;

typedef struct mpmcq_dwcas_t
{
  union
  {
    struct
    {
      uint64_t aba;
      mpmcq_node_t* node;
    };

    __int128_t dw;
  };
} mpmcq_dwcas_t __attribute__ ((aligned (16)));

typedef struct mpmcq_t
{
  volatile mpmcq_node_t* head;
  volatile mpmcq_dwcas_t tail;
} mpmcq_t __attribute__ ((aligned (64)));

void mpmcq_init(mpmcq_t* q);

void mpmcq_destroy(mpmcq_t* q);

void mpmcq_push(mpmcq_t* q, void* data);

void* mpmcq_pop(mpmcq_t* q);

#endif
