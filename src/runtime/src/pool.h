#ifndef pool_h
#define pool_h

#include <stddef.h>

// 64 128 256 512 1024 2048 4096 8192 16k 32k

#define POOL_SIZE_BITS 16
#define POOL_MIN_BITS 6
#define POOL_COUNT (POOL_SIZE_BITS - POOL_MIN_BITS)

typedef volatile __int128_t pool_aba_t;

typedef struct pool_thread_t
{
  void* pool;
  size_t length;
  void* start;
  void* end;
} pool_thread_t;

typedef struct pool_global_t
{
  size_t size;
  size_t count;
  pool_aba_t central;
} pool_global_t;

extern pool_global_t pool_global[POOL_COUNT];
extern __thread pool_thread_t pool_thread[POOL_COUNT];

void* pool_alloc(pool_thread_t* thread, pool_global_t* global);
void pool_free(void* p, pool_thread_t* thread, pool_global_t* global);

#define POOL_TYPEINDEX(SIZE) \
  __builtin_choose_expr(SIZE <= (1 << (POOL_MIN_BITS + 0)), 0, \
  __builtin_choose_expr(SIZE <= (1 << (POOL_MIN_BITS + 1)), 1, \
  __builtin_choose_expr(SIZE <= (1 << (POOL_MIN_BITS + 2)), 2, \
  __builtin_choose_expr(SIZE <= (1 << (POOL_MIN_BITS + 3)), 3, \
  __builtin_choose_expr(SIZE <= (1 << (POOL_MIN_BITS + 4)), 4, \
  __builtin_choose_expr(SIZE <= (1 << (POOL_MIN_BITS + 5)), 5, \
  __builtin_choose_expr(SIZE <= (1 << (POOL_MIN_BITS + 6)), 6, \
  __builtin_choose_expr(SIZE <= (1 << (POOL_MIN_BITS + 7)), 7, \
  __builtin_choose_expr(SIZE <= (1 << (POOL_MIN_BITS + 8)), 8, \
  __builtin_choose_expr(SIZE <= (1 << (POOL_MIN_BITS + 9)), 9, \
    ((void)0) \
    ))))))))))

#define POOL_TYPE(TYPE) \
  POOL_TYPEINDEX(sizeof(TYPE))

#define POOL_ALLOCINDEX(INDEX) \
  pool_alloc(&pool_thread[INDEX], &pool_global[INDEX])

#define POOL_ALLOCSIZE(SIZE) \
  POOL_ALLOCINDEX(__builtin_ffsl(SIZE) - (POOL_MIN_BITS + 1))

#define POOL_ALLOC(TYPE) \
  POOL_ALLOCINDEX(POOL_TYPE(TYPE))

#define POOL_FREEINDEX(INDEX, VALUE) \
  pool_free(VALUE, &pool_thread[INDEX], &pool_global[INDEX])

#define POOL_FREESIZE(SIZE, VALUE) \
  POOL_FREEINDEX(__builtin_ffsl(SIZE) - (POOL_MIN_BITS + 1), VALUE)

#define POOL_FREE(TYPE, VALUE) \
  POOL_FREEINDEX(POOL_TYPE(TYPE), VALUE)

#endif
