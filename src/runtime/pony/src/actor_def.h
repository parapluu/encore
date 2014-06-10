/* 
 * File:   actor_def.h
 * Author: tobias
 *
 * Created on den 6 juni 2014, 01:53
 */

#ifndef ACTOR_DEF_H
#define	ACTOR_DEF_H

#ifdef	__cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdint.h>
#include "heap.h"
#include "map.h"
#include "actorq.h"

struct pony_actor_t
{
  void* p;
  uint32_t rc;
  uint32_t thread;
  bool blocking_on_a_future; // XXX: Refactor 
  bool blocked;
  bool mark;
  bool refchanged;

  heap_t heap;
  map_t* foreign_ref;
  map_t* local_ref;

  // keep things accessed by other actors on a separate cache line
  actorq_t q __attribute__ ((aligned (64)));
  pony_actor_type_t* actor_type;
};

#ifdef	__cplusplus
}
#endif

#endif	/* ACTOR_DEF_H */

