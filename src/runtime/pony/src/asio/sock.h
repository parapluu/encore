#ifndef asio_sock_h
#define asio_sock_h

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <sys/uio.h>
#include <arpa/inet.h>

#include "streambuf.h"

/** Opaque definition of a socket.
 *
 *  This type is the runtime representation
 *  of network sockets. PonySocks of the high-level
 *  language may use this interface through a C-FFI.
 */
typedef struct sock_t sock_t;

/** Definition of a peer address
 *
 */
typedef struct sock_addr_t
{
  char ipstr[INET6_ADDRSTRLEN];
  uint32_t port;
} sock_addr_t;

/** Create a socket for a specific service.
 *
 *  The underlying socket is created
 *  for a given protocol. Any socket
 *  is created as stream and socket
 *  as in non-blocking. Blocking
 *  sockets are not supported.
 *
 *  Returns a pointer to a socket or NULL
 *  if an error occured.
 *
 *  So far, proto is ignored, may be later
 *  used as compile flag TCP vs. InfiniBand.
 */
sock_t* sock_create(uint32_t proto, char* service);

/** Retrieves the OS-level file descriptor
 *  of a given socket.
 */
intptr_t sock_get_fd(sock_t* sock);

/** Set peer address of given socket.
 *
 */
void sock_set_addr(sock_t* sock, char* ipstr, uint32_t port);

/** Retrieves the connection info of the remote peer
 *  associated with a given socket.
 *
 *  For listener sockets, the provided IP string and port
 *  is that of the local network interface on which we
 *  listen for incomming connections.
 */
sock_addr_t* sock_get_addr(sock_t* sock);

/** Listen for incomming connections on the service
 *  that this socket is bound to.
 *
 *  The backlog determines the size of the buffer
 *  for incomming connections. Once the buffer
 *  is exhausted, the underlying protocol triggers
 *  reconnects. Hence, the backlog parameter may
 *  be set wisely.
 *
 *  Returns 0 on success and ASIO_ERROR if an error
 *  occurded. ERRNO is set appropriately.
 */
uint32_t sock_listen(sock_t* sock, uint32_t backlog);

/** Accepts a new connection.
 *
 *  The resulting socket is internally allocated
 *  and stored at dest.
 *
 *  Returns 0 on success, ASIO_WOULDBLOCK
 *  if the requested operation would block
 *  or ASIO_ERROR if an error occured.
 */
uint32_t sock_accept(sock_t* src, sock_t** dest);

/** Connect to a remote peer.
 *
 *  Return 0 on success, ASIO_WOULDBLOCK
 *  or ASIO_ERROR if an error occured.
 *
 *  If a call to connect would block,
 *  the caller should subscribe for
 *  writability.
 */
uint32_t sock_connect(sock_t** sock, char* host, char* port);

/** Close a socket.
 *
 *  Closing a socket does NOT cause I/O events
 *  correlated to that socket to be unsubscribed.
 */
void sock_close(sock_t* sock);

/** Convenience function for sock_writev.
 *
 */
uint32_t sock_write(sock_t* s, streambuf_t* sb);

/** Read data from a socket.
 *
 *  Returns ASIO_SUCCESS on success, ASIO_WOULDBLOCK
 *  if the requested operation would block
 *  ASIO_CLOSED if the remote peer closed
 *  the connection or ASIO_ERROR if an error
 *  occured.
 *
 *  The number of bytes read is written to nrp.
 */
uint32_t sock_read(sock_t* s, struct iovec* iov, size_t chunks, size_t* nrp);

#endif
