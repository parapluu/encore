#include "event.h"
#include "../actor/actor.h"
#include "../mem/pool.h"

#include <string.h>

asio_event_t* asio_event_create(intptr_t fd, uint32_t eflags, uint64_t msg_id,
	bool noisy, void* udata)
{
	asio_event_t* e = POOL_ALLOC(asio_event_t);

	e->fd = fd;
	e->eflags = eflags;
	e->msg_id = msg_id;
	e->noisy = noisy;
	e->udata = udata;

	e->owner = actor_current();

  //TODO FIX: only if this is the first I/O registration
	actor_inc_rc(e->owner);

	return e;
}

void asio_event_dtor(asio_event_t** ev)
{
	//TODO FIX: only if this is the actors last event
	actor_dec_rc((*ev)->owner);

	POOL_FREE(asio_event_t, *ev);
	*ev = NULL;
}
