#ifndef asio_streambuf_h
#define asio_streambuf_h

#include <sys/uio.h>
#include <stddef.h>
#include <stdbool.h>

/** Opaque definition of a streambuf_t.
 *
 */
typedef struct streambuf_t streambuf_t;

/** Get the amount of bytes readable.
 *
 */
size_t streambuf_length(streambuf_t* buf);

/** Returns the number of chunks maintained by the
 *  stream buffer.
 */
size_t streambuf_chunks(streambuf_t* buf);

/** Writes data to a given stream buffer.
 *
 */
void streambuf_write(streambuf_t** buf, void* data, size_t len);

/** Reads data from a given stream buffer.
 *
 */
streambuf_t* streambuf_read(streambuf_t* buf, size_t bytes, void* to);

/** Vectorizes a given stream buffer.
 *
 *  Assumes that iov points to a sufficiently
 *  large array of iovecs.
 */
void streambuf_vectorize(streambuf_t* buf, struct iovec* iov);

/** Provides a base pointer to some amount of free bytes.
 *
 *  The amount of bytes free is written to avail.
 *
 *  Returns a base pointer to the available memory.
 */
void* streambuf_get_space(streambuf_t** buf, size_t* avail);

/** Forwards internal write pointers of a stream buffer by a given number
 *  of bytes.
 *
 *  Returns true if the end of the buffer is reached.
 */
bool streambuf_write_advance(streambuf_t* buf, size_t bytes);

/** Forwards internal read pointers of a stream buffer by a given number
 *  of bytes.
 *
 *  Returns true if the end of the buffer is reached.
 */
bool streambuf_read_advance(streambuf_t* buf, size_t bytes);

/** Free a stream buffer.
 *
 *  Internally maintained data is deallocated.
 */
void streambuf_free(streambuf_t** buf);


#endif
