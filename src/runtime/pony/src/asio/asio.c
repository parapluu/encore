#include <pthread.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>

#include "asio.h"
#include "../mem/pool.h"


typedef ssize_t op_fn(int fd, const struct iovec *iov, int iovcnt);

struct asio_base_t
{
	pthread_t tid;
	asio_backend_t* backend;
	uint32_t subscriptions;
};

static asio_base_t* running_base;

/** Start an asynchronous I/O event mechanism.
 *
 *  Errors are always delegated to the owning actor
 *  of an I/O subscription and never handled within
 *  the runtime system.
 *
 *  In any case (independent of the underlying backend)
 *  only one I/O dispatcher thread will be started.
 *  Since I/O events are subscribed by actors, we do not need
 *  to maintain a thread pool. Instead, I/O is processed in
 *  the context of the owning actor.
 */
static void start()
{
  asio_base_t* new_base = POOL_ALLOC(asio_base_t);
	memset((void*)new_base, 0, sizeof(asio_base_t));

	new_base->backend = asio_backend_init();

	asio_base_t* existing = NULL;

	if(__atomic_compare_exchange_n(&running_base, &existing, new_base, false,
		__ATOMIC_RELAXED, __ATOMIC_RELAXED))
	{
		uint32_t rc = 0;
		if((rc = pthread_create(&running_base->tid, NULL, asio_backend_dispatch,
			running_base->backend)) != 0)
		  exit(EXIT_FAILURE);

  	pthread_detach(running_base->tid);
	}
	else
	{
		asio_backend_terminate(new_base->backend);
		POOL_FREE(asio_base_t, new_base);
	}
}

/** Wrapper for writev and readv.
 */
static uint32_t exec(op_fn* fn, intptr_t fd, struct iovec* iov, size_t* chunks,
	size_t* nrp)
{
	ssize_t ret;
	*nrp = 0;

  while(*chunks > 0)
  {
		ret = fn(fd, iov, *chunks);

    if(ret < 0 && errno != EWOULDBLOCK)
			return ASIO_ERROR;
	  else if(ret < 0 && errno == EWOULDBLOCK)
			return ASIO_WOULDBLOCK;

    if(nrp != NULL) *nrp += ret;

	  while(ret > 0)
    {
	    if(ret >= iov->iov_len)
      {
        ret -= iov->iov_len;
        *chunks -= 1;
        iov++;
      }
      else
      {
        iov->iov_len -= ret;
        iov->iov_base += ret;
        ret = 0;
      }
    }
  }

  return ASIO_SUCCESS;
}

asio_backend_t* asio_get_backend()
{
	if(running_base == NULL) start();
	return running_base->backend;
}

bool asio_stop()
{
	if(running_base->subscriptions > 0)
		return false;

  //TODO FIX: this is not thread safe
  asio_backend_terminate(running_base->backend);

	return true;
}

uint32_t asio_writev(intptr_t fd, struct iovec* iov, size_t* chunks, size_t* nrp)
{
	return exec(writev, fd, iov, chunks, nrp);
}

uint32_t asio_readv(intptr_t fd, struct iovec* iov, size_t chunks, size_t* nrp)
{
	return exec(readv, fd, iov, &chunks, nrp);
}
