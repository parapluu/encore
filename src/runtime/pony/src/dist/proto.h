#ifndef dist_proto_h
#define dist_proto_h

#include <stdbool.h>

#include "../asio/sock.h"
#include "../asio/streambuf.h"

enum PROTO_MSG_T
{
	CONTROL_NODE_ID           = 0x0001,
	CONTROL_NODE_ACCEPTED     = 0x0002,
	CONTROL_NODE_DELEGATE     = 0x0004,
	CONTROL_NODE_PEER_ADDR    = 0x0008,
	CONTROL_NODE_ADVERTISE    = 0x0010,
	CONTROL_CORE_REGISTER     = 0x0020,
	CONTROL_ACTOR_MIGRATION   = 0x0040,
	CONTROL_CLUSTER_TERMINATE = 0x0080,
	APP_ACTOR_MSG             = 0x0100,
	APP_CYCLE_UNBLOCK         = 0x0200,
	APP_CYCLE_BLOCK           = 0x0400,
	APP_CYCLE_CONFIRM         = 0x0800,
	APP_ACTOR_RC              = 0x1000,

	CONTROL_DIST_QUIESCENCE   = (1 << 14),
	PROTO_NOP								 = (1 << 15)
};

/** Opaque definition of a protocol (for receiving
 *  only).
 *
 */
typedef struct proto_t proto_t;

/** Receive data from a socket.
 *
 *  Returns the TYPE of a received message.
 */
uint32_t proto_receive(proto_t** p, sock_t* s, streambuf_t** sb);

/** Send data to a remote peer.
 *
 *  Returns the ASIO state of the operation.
 */
uint32_t proto_send(sock_t* s, uint16_t msg, streambuf_t* sb);

/** Continue a previously started communication phase
 *  that blocked.
 */
uint32_t proto_continue(sock_t* s);

#endif
