#ifndef heap_h
#define heap_h

#include <pony/pony.h>
#include <stdlib.h>
#include <stdbool.h>

#define HEAP_MINBITS 6
#define HEAP_MAXBITS 11
#define HEAP_SIZECLASSES (HEAP_MAXBITS - HEAP_MINBITS + 1)

typedef struct chunk_t chunk_t;

typedef struct heap_t
{
  chunk_t* small[HEAP_SIZECLASSES];
  chunk_t* small_full[HEAP_SIZECLASSES];
  chunk_t* large;

  size_t used;
  size_t next_gc;
} heap_t;

void heap_init(heap_t* heap);

void heap_destroy(heap_t* heap);

void* heap_alloc(pony_actor_t* actor, heap_t* heap, size_t size)
  __attribute__((malloc,alloc_size(3)));

bool heap_startgc(heap_t* heap);

bool heap_mark(chunk_t* chunk, void* p);

void heap_endgc(heap_t* heap);

pony_actor_t* heap_owner(chunk_t* chunk);

#endif
