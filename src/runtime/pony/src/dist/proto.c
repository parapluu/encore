#include "proto.h"
#include "../asio/asio.h"
#include "../mem/pool.h"

#include <string.h>
#include <stdio.h>

enum PROTO_INTERNALS
{
	MASK  = 0xF000,
  MAGIC = 0xA000,
  TYPE  = 0x0FFF
};

typedef uint16_t header_t;
typedef uint32_t frame_len_t;

typedef struct prefix_t {
	header_t header;
	frame_len_t length;
} prefix_t;

struct proto_t
{
	prefix_t prefix;
	bool header_complete;
};

static void reset_prefix(proto_t* p)
{
	p->prefix.header = 0;
	p->prefix.length = 0;
	p->header_complete = false;
}

static uint32_t read_data(sock_t* s, streambuf_t** sb, size_t* nrp)
{
	uint32_t rc = 0;
	size_t avail = 0;
	struct iovec iov[1];

	iov[0].iov_base = streambuf_get_space(sb, &avail);
	iov[0].iov_len = avail;

	rc = sock_read(s, iov, 1, &avail);

	streambuf_write_advance(*sb, avail);

	*nrp = streambuf_length(*sb);

	return rc;
}

static void read_header(proto_t* p, streambuf_t* sb)
{
	streambuf_read(sb, sizeof(header_t) + sizeof(frame_len_t), p);
	p->header_complete = true;
}

static header_t get_message(proto_t* p, streambuf_t** sb)
{
	header_t type = 0;

	if((p->prefix.header & MASK) != MAGIC)
	{
		streambuf_free(sb);
		return PROTO_NOP;
	}

  type = (p->prefix.header & TYPE);
	reset_prefix(p);

	return type;
}

static bool check_for_message(proto_t* p, streambuf_t* sb)
{
	if(p == NULL) return false;

	size_t avail = streambuf_length(sb);
	size_t req = sizeof(header_t) + sizeof(frame_len_t);

	if(avail >= req && !p->header_complete) read_header(p, sb);

  if(avail >= (req + p->prefix.length)) return true;

	return false;
}

uint32_t proto_receive(proto_t** p, sock_t* s, streambuf_t** sb)
{
	uint32_t rc = 0;
	size_t avail = 0;

	proto_t* pt = *p;

	//is there a message pending in the stream buffer?
	if(check_for_message(pt, *sb)) return get_message(pt, sb);

	if(pt == NULL)
	{
		pt = *p = POOL_ALLOC(proto_t);

    reset_prefix(pt);
	}

	while(true)
	{
		rc = read_data(s, sb, &avail);

		if(avail == 0 && rc & (ASIO_ERROR | ASIO_WOULDBLOCK)) return PROTO_NOP; //TODO: ASIO_ERROR OR ASIO_WOULDBLOCK?

    if(check_for_message(pt, *sb)) return get_message(pt, sb);
	}
}

uint32_t proto_send(sock_t* s, uint16_t msg, streambuf_t* sb)
{
	uint32_t rc = 0;
	streambuf_t* frame = NULL;

	prefix_t prefix;

	prefix.header = (MAGIC | msg);
	prefix.length = (sb == NULL) ? 0 : streambuf_length(sb);

	streambuf_write(&frame, &prefix.header, sizeof(prefix.header));
	streambuf_write(&frame, &prefix.length, sizeof(prefix.length));

	rc |= sock_write(s, frame);
  if(sb != NULL) rc |= sock_write(s, sb);

	return rc;
}

uint32_t proto_continue(sock_t* s)
{
	return sock_write(s, NULL);
}
