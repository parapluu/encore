#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>

#include "bitset.h"

// ================================================================
// bitset.c 
// Implements a set of integers using bit operations
// Written to demonstrate:
// - bit operations
// - variable argument lists
// - macros
// - consts
// - function pointers (a bit contrieved)
// ================================================================

/* ================================================================
   TODO:
   [x] Define the set data structure as a sequence of N bits
   [x] Define a constructor for sets
   [x] Operations on individual bits:
       [x] Set a bit 
       [x] Unset a bit
       [x] Read a bit
       [x] Show alternative implementation with enums (maybe messier)
   [x] Operations on entire bit sets:
       [x] Set union
       [x] Set intersection
       [x] Set xor 
   [x] Defensive programming with asserts
   [x] Define a print operation on sets { 1, 3, 9, 12, ... }
   [x] Add const declarations in suitable places
   [x] Rewrite _setop to work with function pointers instead
   ================================================================ */

typedef bitset(*binop_bitset)(bitset, bitset);
typedef enum { set_op, unset_op, read_op } bit_op; 
typedef enum { union_op, intersection_op, xor_op } bitset_op; 

// 16777216
#define ELEMENTS          268435456
#define BITS_PER_ELEMENT  (sizeof(bitset) * 8)
#define ARRAY_SIZE        (ELEMENTS / BITS_PER_ELEMENT)
#define ONE               1UL

#define mask(num)          (ONE << (num % BITS_PER_ELEMENT)) 
#define element(bits, num) (bits[num / BITS_PER_ELEMENT])

#define assertTrue(arg, msg) \
  do { \
    if (!(arg)) {							\
      printf("%s:%d Failed: (%s) %s\n", __FILE__, __LINE__, #arg, msg); \
      abort(); \
    } \
  } while (0); 
#define assertFalse(arg, msg) assertTrue((!arg), msg); 
#define fail(msg)             assertTrue(0, msg); 

bitset *mkBitset(int64_t element_count, ...) {
  assertTrue(element_count >= 0, "mkBitset called with negative number of elements");
  assertTrue(element_count < ELEMENTS, "mkBitset called with too many elements");

  bitset *bits = calloc(ARRAY_SIZE, sizeof(bitset));
  assertTrue(bits, "NULL result from calloc -- probably ran out of memory");

  va_list elements;
  va_start(elements, element_count);
  while(element_count--) {
    set(bits, va_arg(elements, int64_t)); 
  }
  va_end(elements);

  return bits;
}

static inline bitset *_setop_fun(const bitset *lhs, binop_bitset binop, const bitset *rhs) {
  bitset *result = mkBitset(0); 
  for (int64_t i = 0; i < ELEMENTS; ++i) { 
    result[i] = binop(lhs[i], rhs[i]); 
  }
  return result;
}

void print(bitset* bits) {
  printf("{ ");
  char *pad = "";
  for (int64_t i = 0; i < ELEMENTS; ++i) {
    if (isset(bits, i)) {
      printf("%s%llu", pad, i);
      pad = ", ";
    }
  }
  printf(" }\n");
}

void set (bitset *bits, int64_t index) { 
  assertTrue(index >= 0, "Negative numbers not allowed");
  assertTrue(index < ELEMENTS, "index exceeds size of bitset");
  element(bits, index) |=  mask(index); 
}

void unset (bitset *bits, int64_t index) { 
  assertTrue(index >= 0, "Negative numbers not allowed");
  assertTrue(index < ELEMENTS, "index exceeds size of bitset");
  element(bits, index) &= ~mask(index); 
}

int64_t isset (bitset *bits, int64_t index) { 
  assertTrue(index >= 0, "Negative numbers not allowed");
  assertTrue(index < ELEMENTS, "index exceeds size of bitset");
  return element(bits, index) & mask(index); 
}

bitset binop_and(bitset a, bitset b) { return a & b; }
bitset binop_or (bitset a, bitset b) { return a | b; }
bitset binop_xor(bitset a, bitset b) { return a ^ b; }

bitset *setAnd (const bitset *lhs, const bitset *rhs) { return _setop_fun(lhs, binop_and, rhs ); }
bitset *setOr  (const bitset *lhs, const bitset *rhs) { return _setop_fun(lhs, binop_or,  rhs ); }
bitset *setXor (const bitset *lhs, const bitset *rhs) { return _setop_fun(lhs, binop_xor, rhs ); }

