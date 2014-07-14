#include "asio.h"
#include "event.h"
#ifdef ASIO_USE_KQUEUE

#include "../mem/pool.h"
#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>
#include <string.h>
#include <stdbool.h>

asio_backend_t* asio_backend_init()
{
	return 0;
}

void asio_backend_terminate(asio_backend_t* b)
{

}

void* asio_backend_dispatch(void* arg)
{
  return NULL;
}

void asio_event_subscribe(asio_event_t* ev)
{

}

void asio_event_unsubscribe(asio_event_t* ev)
{

}

#endif
