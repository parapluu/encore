#ifndef __bitset__
#define __bitset__

#include <stdint.h>

// Should be opaque -- i.e., should not be defined here
typedef unsigned long bitset;

// Operations on individual bits in a set
void     set    (bitset *bits, int64_t index);
void     unset  (bitset *bits, int64_t index);
int64_t isset   (bitset *bits, int64_t index);

// Operations on entire sets
bitset *setAnd (const bitset *bits, const bitset *moreBits); 
bitset *setOr  (const bitset *bits, const bitset *moreBits);
bitset *setXor (const bitset *bits, const bitset *moreBits);

// Creator and inspector
bitset *mkBitset(int64_t element_count, ...);
void    print(bitset* bits);

#endif
