#include "sock.h"
#include "asio.h"
#include "../ds/list.h"
#include "../mem/pool.h"

#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <netdb.h>
#include <errno.h>
#include <stdio.h>

#ifndef SOCK_NONBLOCK
#  define SOCK_NONBLOCK 0
#endif

DECLARE_LIST(writeq, streambuf_t);
DEFINE_LIST(writeq, streambuf_t, NULL, NULL);

struct sock_t
{
	intptr_t fd;       /* OS-level file descriptor */
  sock_addr_t addr;  /* Connection info of remote peer */

	uint32_t status;    /* ASIO_CLOSED, ASIO_READABLE, ... */
  writeq_t* w_head;   /* list of pending writes */
  writeq_t* w_tail;

  bool writable; /* WRITE ASIO_WOULDBLOCK BEFORE? */
};

static void set_non_blocking(intptr_t fd)
{
	uint32_t flags = 0;

	#if defined(__linux__) || defined(__APPLE__)
	flags = fcntl(fd, F_GETFL, 0);
	flags |= O_NONBLOCK;
	fcntl(fd, F_SETFL, flags);
	#endif //TODO: windows
}

// deals with both IPv4 and IPv6
static void set_local_addr(sock_t* sock)
{
  socklen_t len;
  struct sockaddr_storage addr;

  len = sizeof(addr);

	//if(listener)
  getsockname(sock->fd, (struct sockaddr*)&addr, &len);
	//else
	//	getpeername(sock->fd, (struct sockaddr*)&addr, &len);

  if (addr.ss_family == AF_INET) {
    struct sockaddr_in *s = (struct sockaddr_in *)&addr;
    sock->addr.port = ntohs(s->sin_port);
    inet_ntop(AF_INET, &s->sin_addr, sock->addr.ipstr, sizeof(sock->addr.ipstr));
  }
  else
  {
    struct sockaddr_in6 *s = (struct sockaddr_in6 *)&addr;
    sock->addr.port = ntohs(s->sin6_port);
    inet_ntop(AF_INET6, &s->sin6_addr, sock->addr.ipstr, sizeof(sock->addr.ipstr));
  }
}

sock_t* sock_create(uint32_t proto, char* service)
{
  sock_t* s = POOL_ALLOC(sock_t);
  uint32_t rc = 0;
  uint32_t option = 1;
  struct addrinfo hints, *res, *curr;

  memset(&hints, 0, sizeof(struct addrinfo));
  memset(s, 0, sizeof(sock_t));

  hints.ai_family   = AF_UNSPEC;     /* Allow IPv4 or IPv6 */
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags    = AI_PASSIVE;    /* wildcard IP */
  hints.ai_protocol = IPPROTO_TCP;

  rc = getaddrinfo(NULL, service, &hints, &res);

  if(rc != 0)
  	return NULL;

  for(curr = res; curr != NULL; curr = curr->ai_next)
  {
  	if((s->fd = socket(curr->ai_family, curr->ai_socktype | SOCK_NONBLOCK, curr->ai_protocol)) == -1)
  		continue;

    setsockopt(
			s->fd,
			SOL_SOCKET,
			SO_REUSEADDR,
			&option,
			sizeof(option)
		);

    if(bind(s->fd, curr->ai_addr, curr->ai_addrlen) == 0)
  		break;

  	close(s->fd);
  }

  if(curr == NULL)
  {
    freeaddrinfo(res);
  	return NULL;
  }

  #if !SOCK_NONBLOCK
  set_non_blocking(s->fd);
  #endif

  freeaddrinfo(res);

  s->writable = true;

	return s;
}

intptr_t sock_get_fd(sock_t* sock)
{
	return sock->fd;
}

sock_addr_t* sock_get_addr(sock_t* sock)
{
  return &sock->addr;
}

uint32_t sock_listen(sock_t* sock, uint32_t backlog)
{
	if(listen(sock->fd, backlog) != 0)
		return ASIO_ERROR;

  sock->status |= ASIO_LISTENING;

  //keep track of the device we are listening on
  set_local_addr(sock);

	return ASIO_SUCCESS;
}

uint32_t sock_accept(sock_t* src, sock_t** dest)
{
  sock_t* s = POOL_ALLOC(sock_t);
  memset(s, 0, sizeof(sock_t));

  s->fd = accept(src->fd, NULL, NULL);

	if(s->fd == -1)
	{
    POOL_FREE(sock_t, s);

    if(errno == EWOULDBLOCK || errno == EAGAIN)
		  return ASIO_WOULDBLOCK;
		else
			return ASIO_ERROR;
	}

  set_non_blocking(s->fd);

  s->writable = true;
  *dest = s;

	return ASIO_SUCCESS;
}

uint32_t sock_connect(sock_t** sock, char* host, char* service)
{
  struct addrinfo *res, *curr;

  sock_t* s = POOL_ALLOC(sock_t);
  memset(s, 0, sizeof(sock_t));

	uint32_t rc = getaddrinfo(host, service, NULL, &res);

	if(rc != 0)
  {
    freeaddrinfo(res);
		return ASIO_ERROR;
  }

  for(curr = res; curr != NULL; curr = curr->ai_next)
  {
    if((s->fd = socket(curr->ai_family, curr->ai_socktype | SOCK_NONBLOCK, curr->ai_protocol)) == -1)
      continue;

  	if(connect(s->fd, curr->ai_addr, curr->ai_addrlen) != -1 || errno == EINPROGRESS)
  		break; /* connection success, but may be pending */
  }

  if(curr == NULL)
  {
    POOL_FREE(sock_t, s);
    freeaddrinfo(res);

  	return ASIO_ERROR;
  }

  #if !SOCK_NONBLOCK && defined(FIONBIO)
  option = 1;
  ioctl(s->fd, FIONBIO, &option);
  #endif

  freeaddrinfo(res);

  *sock = s;
  s->writable = true;

	return errno == EINPROGRESS ? ASIO_WOULDBLOCK : ASIO_SUCCESS;
}

void sock_close(sock_t* sock)
{
  if(sock->w_head != NULL) return;

	close(sock->fd);
	POOL_FREE(sock_t, sock);
}


uint32_t sock_write(sock_t* s, streambuf_t* sb)
{
  streambuf_t* h = NULL;

  size_t chunks = 0;
  size_t nrp = 0;
  uint32_t rc = 0;

  if(sb == NULL && s->w_tail == NULL) return ASIO_SUCCESS;

  if(sb != NULL) s->w_tail = writeq_append(s->w_tail, sb);

  if(s->w_head == NULL) s->w_head = s->w_tail;

  if(s->writable || sb == NULL)
  {
    while(true)
    {
      s->w_head = writeq_pop(s->w_head, &h);
			struct iovec iov[chunks = streambuf_chunks(h)];

      streambuf_vectorize(h, iov);

      rc = asio_writev(s->fd, iov, &chunks, &nrp);

			if((rc & (ASIO_ERROR | ASIO_WOULDBLOCK)) != 0) break;

      if(!streambuf_read_advance(h, nrp))
      {
        s->w_head = writeq_push(s->w_head, h);
        continue;
      }
      else if(s->w_head == NULL)
      {
        s->w_tail = NULL;
        s->writable = true;

				streambuf_free(&h);

        return ASIO_SUCCESS;
      }

			streambuf_free(&h);
    }
  }

  if(rc & ASIO_WOULDBLOCK) s->writable = false;

  return rc;
}

uint32_t sock_read(sock_t* s, struct iovec* iov, size_t chunks, size_t* nrp)
{
	return asio_readv(s->fd, iov, chunks, nrp);
}
