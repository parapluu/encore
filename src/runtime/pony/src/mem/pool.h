#ifndef mem_pool_h
#define mem_pool_h

#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

#define POOL_MIN_BITS 6

void* pool_alloc(int index);
void pool_free(int index, void* p);

void* pool_alloc_size(size_t size);
void pool_free_size(size_t size, void* p);

bool pool_debug_appears_freed();

#ifndef __cplusplus

#define POOL_INDEX(SIZE) \
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
  __builtin_choose_expr(SIZE <= (1 << (POOL_MIN_BITS + 10)), 10, \
  __builtin_choose_expr(SIZE <= (1 << (POOL_MIN_BITS + 11)), 11, \
  __builtin_choose_expr(SIZE <= (1 << (POOL_MIN_BITS + 12)), 12, \
  __builtin_choose_expr(SIZE <= (1 << (POOL_MIN_BITS + 13)), 13, \
  __builtin_choose_expr(SIZE <= (1 << (POOL_MIN_BITS + 14)), 14, \
    ((void)0) \
    )))))))))))))))

#else

#define POOL_INDEX(SIZE) \
  ( \
  (SIZE <= (1 << (POOL_MIN_BITS + 0))) ? 0 : \
  (SIZE <= (1 << (POOL_MIN_BITS + 1))) ? 1 : \
  (SIZE <= (1 << (POOL_MIN_BITS + 2))) ? 2 : \
  (SIZE <= (1 << (POOL_MIN_BITS + 3))) ? 3 : \
  (SIZE <= (1 << (POOL_MIN_BITS + 4))) ? 4 : \
  (SIZE <= (1 << (POOL_MIN_BITS + 5))) ? 5 : \
  (SIZE <= (1 << (POOL_MIN_BITS + 6))) ? 6 : \
  (SIZE <= (1 << (POOL_MIN_BITS + 7))) ? 7 : \
  (SIZE <= (1 << (POOL_MIN_BITS + 8))) ? 8 : \
  (SIZE <= (1 << (POOL_MIN_BITS + 9))) ? 9 : \
  (SIZE <= (1 << (POOL_MIN_BITS + 10))) ? 10 : \
  (SIZE <= (1 << (POOL_MIN_BITS + 11))) ? 11 : \
  (SIZE <= (1 << (POOL_MIN_BITS + 12))) ? 12 : \
  (SIZE <= (1 << (POOL_MIN_BITS + 13))) ? 13 : \
  (SIZE <= (1 << (POOL_MIN_BITS + 14))) ? 14 : \
  0)

#endif

#define POOL_ALLOC(TYPE) \
  pool_alloc(POOL_INDEX(sizeof(TYPE)))

#define POOL_FREE(TYPE, VALUE) \
  pool_free(POOL_INDEX(sizeof(TYPE)), VALUE)

#ifdef __cplusplus
}
#endif

#endif
