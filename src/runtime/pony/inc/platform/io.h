#ifndef PLATFORM_IO_H
#define PLATFORM_IO_H

/** I/O support.
 *
 */
#ifndef PLATFORM_IS_WINDOWS
#include <sys/uio.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <netdb.h>
#else
struct iovec { void* iov_base; size_t iov_len; };
#endif


#endif
