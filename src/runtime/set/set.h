/**
 *  @file set.h
 * \mainpage Everything you want to know is in set.h
 *
*/

#ifndef __set_h__
#define __set_h__

#include <stdbool.h>
#include "closure.h"

typedef struct set set_t;

typedef void  (*printer_fnc)(void *elem); 
typedef void *(*map_fnc)(void *elem); 
typedef void *(*forall_fnc)(void *elem, void* arg); 
typedef void *(*reduce_fnc)(void *elem, void *accumulator);

/**
 *  Create a new empty set. Use set_destroy"()" to free the allocated memory.
 *  @return A new empty set
 */
set_t *set_mk(void);

/**
 *  Insert an element in a set.
 *  @param set The set to be extended
 *  @param elem The element to be inserted
 *  @return 0 if \p elem was already \p set, otherwise a non-zero value
 */ 
bool set_add(set_t *set, void *elem);

/**
 *  Test an element for set membership.
 *  @param set The set to be searched
 *  @param elem The element sought for
 *  @return 0 if \p elem was not in \p set, otherwise a non-zero value
 */ 
bool set_elem(set_t *set, void *elem);

/**
 *  Remove an element from the set.
 *  @param set The set to be contracted
 *  @param elem the element to be deleted
 *  @return 0 if \p elem was not in \p set, otherwise a non-zero value
 */ 
bool set_remove(set_t *set, void *elem);

/**
 *  Test a set for subset relation.
 *  @param sub
 *  @param super
 *  @return 0 if \p sub is not a subset of \p super, otherwise a non-zero value
 */ 
bool set_subset(set_t *sub, set_t *super);

/**
 *  Test for set equality.
 *  @param set
 *  @param other
 *  @return 0 if \p set and \p other do not contain exactly the same elements, otherwise a non-zero value
 */ 
bool set_eq(set_t *set, set_t *other);

/**
 *  Create a copy of a set.
 *  @param set The set to be cloned
 *  @return A new set with all elements of \p set copied
 */ 
set_t *set_clone(set_t *set);

/**
 *  Print a set. 
 *  @param set The set to be printed
 *  @param print A function that prints a single element of the set
 */ 
void set_print(set_t *set, printer_fnc print);

/**
 *  Remove a set (but not its elements) from memory
 *  @param set The set to be destroyed
 */ 
void set_destroy(set_t *set);

/**
 *  Map over all the elements of a set.
 *  @param set The set to be mapped over
 *  @param f A function that maps (with or without side-effects) a single element of the set to a new one
 *  @return The set {\p f (x) | x <-- \p set}
 */ 
set_t *set_map(set_t *set, map_fnc f);

/**
 *  Like a map over a set where the return type of f is void.
 *  @param set The set to be mapped over
 *  @param f A function that maps (with or without side-effects) a single element of the set to a new one
 *  @param arg An argument to be supplied as the last argument to each f
 */
void set_forall(set_t *set, forall_fnc f, void *arg);

/** 
 * @see set_forall, but uses a closure
 */
void set_forall_closure(set_t *set, closure_t *c);

/**
 *  Reduce a set to a single element
 *  @param set The set to be reduced
 *  @param f A binary function
 *  @param init The initial accumulator value
 *  @return \p f (xn, \p f (..., \p f (x2, \p f (x1, \p init))...))
 */ 
void *set_reduce(set_t *set, reduce_fnc f, void *init);

#endif
