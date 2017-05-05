#ifndef mem_heap_h
#define mem_heap_h

#include "pool.h"
#include "../pony.h"
#include <platform.h>
#include <stdlib.h>
#include <stdbool.h>

PONY_EXTERN_C_BEGIN

#define HEAP_MINBITS 5
#define HEAP_MAXBITS (POOL_ALIGN_BITS - 1)
#define HEAP_SIZECLASSES (HEAP_MAXBITS - HEAP_MINBITS + 1)
#define HEAP_MIN (1 << HEAP_MINBITS)
#define HEAP_MAX (1 << HEAP_MAXBITS)

typedef struct chunk_t chunk_t;

typedef struct heap_t
{
  chunk_t* small_free[HEAP_SIZECLASSES];
  chunk_t* small_full[HEAP_SIZECLASSES];
  chunk_t* large;

  size_t used;
  size_t next_gc;
} heap_t;

uint32_t ponyint_heap_index(size_t size);

void ponyint_heap_setinitialgc(size_t size);

void ponyint_heap_setnextgcfactor(double factor);

void ponyint_heap_init(heap_t* heap);

void ponyint_heap_destroy(heap_t* heap);

void ponyint_heap_final(heap_t* heap);

__pony_spec_malloc__(
  void* ponyint_heap_alloc(pony_actor_t* actor, heap_t* heap, size_t size)
  );

__pony_spec_malloc__(
void* ponyint_heap_alloc_small(pony_actor_t* actor, heap_t* heap,
  uint32_t sizeclass)
  );

__pony_spec_malloc__(
void* ponyint_heap_alloc_large(pony_actor_t* actor, heap_t* heap, size_t size)
  );

void* ponyint_heap_realloc(pony_actor_t* actor, heap_t* heap, void* p,
  size_t size);

__pony_spec_malloc__(
  void* ponyint_heap_alloc_final(pony_actor_t* actor, heap_t* heap, size_t size)
  );

__pony_spec_malloc__(
void* ponyint_heap_alloc_small_final(pony_actor_t* actor, heap_t* heap,
  uint32_t sizeclass)
  );

__pony_spec_malloc__(
void* ponyint_heap_alloc_large_final(pony_actor_t* actor, heap_t* heap,
  size_t size)
  );

/**
 * Adds to the used memory figure kept by the heap. This allows objects
 * received in messages to count towards the GC heuristic.
 */
void ponyint_heap_used(heap_t* heap, size_t size);

bool ponyint_heap_startgc(heap_t* heap);

/**
 * Mark an address in a chunk. Returns true if it was already marked, or false
 * if you have just marked it.
 */
bool ponyint_heap_mark(chunk_t* chunk, void* p);

/**
 * Marks an address, but does not affect the return value of ponyint_heap_mark()
 * for the same address, nor does it indicate previous mark status.
 */
void ponyint_heap_mark_shallow(chunk_t* chunk, void* p);

/**
 * Returns true if the address is marked (allocated).
 */
bool ponyint_heap_ismarked(chunk_t* chunk, void* p);

/**
 * Forcibly free this address.
 */
void ponyint_heap_free(chunk_t* chunk, void* p);

void ponyint_heap_endgc(heap_t* heap);

pony_actor_t* ponyint_heap_owner(chunk_t* chunk);

size_t ponyint_heap_size(chunk_t* chunk);

PONY_EXTERN_C_END

#endif
