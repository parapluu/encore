#ifndef MAIN_H
#define MAIN_H

#include <pony/pony.h>
#include <sys/time.h>

void main_streamerdone(pony_actor_t* main);

void main_updaterdone(pony_actor_t* main, struct timeval* tv);

#endif
