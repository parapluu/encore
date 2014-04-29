#ifndef __set_h__
#define __set_h__

typedef struct set *Set;

typedef void (printer_fnc(void *elem));
typedef void *(map_fnc(void *elem));
typedef void *(reduce_fnc(void *elem, void *accumulator));

// Returns a new empty set
Set set_new(void);

// Returns 1 if elem was added in the set, 0 if it was already there
int set_add(Set set, void *elem);

// Returns 1 if elem is in the set, 0 otherwise
int set_elem(Set set, void *elem);

// Returns 1 if elem was removed from the set, 0 if it was not there
int set_remove(Set set, void *elem);

// Returns 1 if sub is a subset of super, 0 otherwise
int set_subset(Set sub, Set super);

// Returns 1 if set and other contains the same elements, 0 otherwise
int set_eq(Set set, Set other);

// Returns a new set with all elements of set copied (by reference)
Set set_clone(Set set);

// Prints the set using the printer function print
void set_print(Set set, printer_fnc print);

// Removes the set (not its elements) from memory
void set_destroy(Set set);

// Maps f over set and returns the result
Set set_map(Set set, map_fnc f);

// Reduce set with f, using init as initial accumulator value
void *set_reduce(Set set, reduce_fnc f, void *init);

#endif