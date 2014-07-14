#define __STDC_FORMAT_MACROS
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <stddef.h>

#include "dist.h"
#include "proto.h"
#include "../actor/actor.h"
#include "../sched/scheduler.h"
#include "../asio/asio.h"
#include "../asio/event.h"
#include "../asio/sock.h"
#include "../ds/hash.h"
#include "../mem/pool.h"

#define LISTEN_BACKLOG 50

enum
{
	DIST_INIT,
	DIST_JOIN,
	DIST_DELEGATE,
	DIST_IO_NOTIFY,
	DIST_FINISH
};

typedef struct node_t
{
	uint32_t node_id;
	uint32_t core_count;
	sock_t* socket;

  proto_t* proto;
  streambuf_t* in;

	bool temporary;
	bool pending;
} node_t;

typedef struct cluster_t
{
	node_t this;
	node_t parent;
	node_t** children;
	uint32_t free_slots;
	uint32_t accepted;
} cluster_t;

typedef struct dist_t
{
	cluster_t cluster;

	//hash_t* proxy_map;
	//hash_t* local_map;
	//hash_t* object_map;

	uint64_t app_msgs;
	uint64_t app_msgs_fwd;
	uint64_t control_msgs;
	uint64_t failed_steals;
	uint64_t pinned_steals;
	uint64_t actors_sent;
	uint64_t actors_recv;
	uint64_t io_wakeups;
	uint64_t io_blocks;
	uint64_t bytes_sent;
	uint64_t bytes_recv;
	uint64_t term_attempts;

	bool master;
	bool stats;
} dist_t;

static pony_actor_t* dist_actor;
static uint32_t next_node_id;

/*static void print_stats(dist_t* d)
{
  printf("Distribution actor (%s):\n"
    "\tapp_msgs: %"PRIu64"\n"
    "\tapp_msgs_forwarded: %"PRIu64"\n"
    "\tcontrol_msgs: %"PRIu64"\n"
    "\tfailed_steals: %"PRIu64"\n"
    "\tpinned_steals: %"PRIu64"\n"
    "\tactors_sent: %"PRIu64"\n"
    "\tactors_recv: %"PRIu64"\n"
    "\tI/O wakeups: %"PRIu64"\n"
    "\tI/O WOULDBLOCKs: %"PRIu64"\n"
    "\tbytes_sent: %"PRIu64"\n"
    "\tbytes_recv: %"PRIu64"\n"
    "\tterm_attempts: %"PRIu64"\n",
    d->master ? "Master" : "Slave",
    d->app_msgs,
    d->app_msgs_fwd,
    d->control_msgs,
    d->failed_steals,
    d->pinned_steals,
    d->actors_sent,
    d->actors_recv,
    d->io_wakeups,
    d->io_blocks,
    d->bytes_sent,
    d->bytes_recv,
    d->term_attempts
  );
}*/

static void trace_node(void* p)
{
	node_t* child = p;

  pony_trace(child->socket);
	pony_trace(child->proto);
	pony_trace(child->in);
}

static void trace_dist(void* p)
{
  dist_t* this = p;

	trace_node(&this->cluster.this);
	trace_node(&this->cluster.parent);

  //pony_trace(this->proxy_map);
	//pony_trace(this->local_map);
  //pony_trace(this->object_map);

	pony_trace(this->cluster.children);

  for(uint32_t i = 0; i < this->cluster.accepted; i++)
	{
		pony_traceobject(this->cluster.children[i], trace_node);
	}
}

static node_t* get_next_hop(cluster_t* cluster, uint32_t node_id)
{
	return NULL;
}

static void join_node(dist_t* d)
{
	cluster_t* v = &d->cluster;

	streambuf_t* buf_acc;
	streambuf_t* buf_del;

  uint32_t rc = 0;
  uint32_t filter = 0;
	node_t* child = 0;
  sock_t* s = 0;

  while(true)
  {
		buf_acc = NULL;
		buf_del = NULL;

  	rc = sock_accept(v->this.socket, &s);

		if((rc & (ASIO_WOULDBLOCK | ASIO_ERROR)) != 0) break;

    child = POOL_ALLOC(node_t);
		memset(child, 0, sizeof(node_t));

		child->socket = s;

		if(d->master)
		{
			next_node_id++;

			streambuf_write( &buf_acc, &next_node_id, sizeof(next_node_id) );

			rc |= proto_send(
				child->socket,
				CONTROL_NODE_ID,
				buf_acc
			);

			child->node_id = next_node_id;
		}

		if(v->free_slots > 0)
		{
      v->children[v->accepted] = child;
			child->temporary = false;

			rc |= proto_send(
							child->socket,
							CONTROL_NODE_ACCEPTED,
							NULL
						);

			v->free_slots--;
			v->accepted++;
		}
		else
		{
			child->temporary = true;

			node_t* next = get_next_hop( v, next_node_id );
			sock_addr_t* addr = sock_get_addr( next->socket );

      streambuf_write( &buf_del, addr, sizeof(sock_addr_t) );

			rc |= proto_send(
				child->socket,
				CONTROL_NODE_DELEGATE,
				buf_del
			);
		}

		filter = ASIO_FILT_READ;

		if((rc & ASIO_WOULDBLOCK) != 0)
		{
			filter |= ASIO_FILT_WRITE;
		}
		else if(child->temporary)
		{
			sock_close( child->socket );
			POOL_FREE( node_t, child );

			continue;
		}

		asio_event_t* e = asio_event_create(
			sock_get_fd( child->socket ),
			filter,
			DIST_IO_NOTIFY,
			false,
			child
		);

		asio_event_subscribe( e );
  }
}

static void handle_remote_message(dist_t* d, node_t* n, uint16_t type)
{
	uint32_t rc = 0;
	uint32_t cores = 0;
	sock_addr_t* addr = 0;
	streambuf_t* buf_cores = NULL;
	streambuf_t* buf_addr = NULL;

	switch(type)
	{
		case CONTROL_NODE_ID:
		  /* This message must come from the parent.
			 *
			 * Tell it the local address that we are listening on.
			 */
			n->in = streambuf_read( n->in, sizeof(uint32_t), &n->node_id );

			printf("[DIST] GOT NODE ID: %d\n", n->node_id);

			break;

		case CONTROL_NODE_DELEGATE:
		  break;

		case CONTROL_NODE_ACCEPTED:
		  d->cluster.parent.pending = false;

			printf("[DIST] ACCEPTED AT PARENT\n");

			cores = scheduler_cores();
			addr = sock_get_addr(d->cluster.this.socket);

			streambuf_write(&buf_cores, &cores, sizeof(cores));

			rc |= proto_send(
				n->socket,
				CONTROL_CORE_REGISTER,
				buf_cores
			);

			streambuf_write(&buf_addr, addr, sizeof(sock_addr_t));

			rc |= proto_send(
				n->socket,
				CONTROL_NODE_PEER_ADDR,
				buf_addr
			);

			// TODO WRITABILITY?

		  break;

		case CONTROL_NODE_PEER_ADDR:
		  /* Message from an accepted child node.
			 *
			 * Remember its listener address and port
			 * for join-delegation.
			 */
 		 addr = sock_get_addr( n->socket );
      n->in = streambuf_read( n->in, sizeof(sock_addr_t), addr );

			printf("[DIST] ACCEPTED NODE %d AT %s:%d\n",
				n->node_id, addr->ipstr, addr->port);

		  break;

		case CONTROL_CORE_REGISTER:
			n->in = streambuf_read( n->in, sizeof(uint32_t), &n->core_count);

			printf("[DIST] NODE %d REPORTED %d CORES\n", n->node_id, n->core_count);
		  break;
	}

	if(type <= CONTROL_CLUSTER_TERMINATE) d->control_msgs += 1;
	else d->app_msgs += 1;
}

static pony_msg_t m_dist_init = {3, {PONY_NONE}};
static pony_msg_t m_dist_join = {2, {PONY_NONE}};
static pony_msg_t m_dist_delegate = {2, {PONY_ACTOR, PONY_NONE}};
static pony_msg_t m_dist_io_notify = {1, {PONY_NONE}};
static pony_msg_t m_dist_finish = {1, {PONY_NONE}};

static pony_msg_t* message_type(uint64_t id)
{
	switch(id)
	{
		case DIST_INIT: return &m_dist_init;
		case DIST_JOIN: return &m_dist_join;
		case DIST_DELEGATE: return &m_dist_delegate;
		case DIST_IO_NOTIFY: return &m_dist_io_notify;
		case DIST_FINISH: return &m_dist_finish;
	}

	return NULL;
}

static void dispatch(pony_actor_t* this, void* p, uint64_t id,
	int argc, pony_arg_t* argv)
{
	dist_t* d = p;

	switch(id)
	{
		case DIST_INIT:
		{
		  d = pony_alloc(sizeof(dist_t));
			memset(d, 0, sizeof(dist_t));

	    d->cluster.children = pony_alloc(sizeof(node_t*)*argv[1].i);

		  d->cluster.free_slots = argv[1].i;
		  d->master = argv[2].i != 0;

		  d->cluster.this.socket = sock_create(0, argv[0].p);

			if(d->cluster.this.socket != NULL)
			{
			  sock_listen(d->cluster.this.socket, LISTEN_BACKLOG);

	      asio_event_t* e = asio_event_create(
					sock_get_fd(d->cluster.this.socket),
	      	ASIO_FILT_READ,
	      	DIST_IO_NOTIFY,
	      	false,
	      	&d->cluster.this
	      );

	      asio_event_subscribe(e);
			}

		  pony_set(d);
		  break;
		}
		case DIST_JOIN:
		{
			d->cluster.parent.temporary = false;
			d->cluster.parent.pending = true;

		  uint32_t rc = sock_connect(&d->cluster.parent.socket,
				argv[0].p, argv[1].p);
			uint32_t flt = ASIO_FILT_READ;

		  if(rc == ASIO_WOULDBLOCK) flt |= ASIO_FILT_WRITE;

      asio_event_t* e = asio_event_create(
				sock_get_fd(d->cluster.parent.socket),
				flt,
			  DIST_IO_NOTIFY,
			  false,
				&d->cluster.parent
		  );

			asio_event_subscribe(e);
		  break;
		}
		case DIST_DELEGATE:
		  //TODO
		  break;
		case DIST_IO_NOTIFY:
		{
			asio_event_t* e = argv[0].p;
      node_t* n = e->udata;
      uint32_t t = argv[1].i;
			uint16_t msg = 0;

			if((t & (ASIO_ERROR | ASIO_PEER_SHUTDOWN | ASIO_CLOSED_UNEXPECTEDLY)) != 0)
			{
      	sock_close(n->socket); // TODO error handling / termination

				POOL_FREE(node_t, n);
				asio_event_unsubscribe(e);
				break;
			}

      if((t & ASIO_READABLE) != 0)
      {
      	if(n == &d->cluster.this)
				{
      		join_node(d);
				}
      	else
      	{
				  if((msg = proto_receive(&n->proto, n->socket, &n->in)) != PROTO_NOP)
					{
						argv[1].i = ASIO_READABLE;

						pony_sendv(this, e->msg_id, 2, argv);
          	handle_remote_message(d, n, msg);
					}
      	}
      }

      if((t & ASIO_WRITABLE) != 0)
			{
				if(proto_continue(n->socket) == ASIO_SUCCESS)
				{
					if(n->temporary)
					{
						asio_event_unsubscribe(e);

						POOL_FREE(node_t, n);
					}
				}
			}

      break;
		}
		case DIST_FINISH:
		  //TODO
		  break;
	}
}

static pony_actor_type_t type =
{
	1, //cycle detector has id 0 (not that it matters :-))
	{sizeof(dist_t), trace_dist, NULL, NULL},
	message_type,
	dispatch,
	NULL
};

void dist_create(char* port, size_t leaf_size, bool master)
{
	pony_arg_t argv[3];
	argv[0].p = port;
	argv[1].i = leaf_size;
	argv[2].i = master;

	dist_actor = pony_create(&type);
	actor_setsystem(dist_actor);
	pony_sendv(dist_actor, DIST_INIT, 3, argv);
}

void dist_join(char* host, char* port)
{
	pony_arg_t argv[2];
	argv[0].p = host;
	argv[1].p = port;

	pony_sendv(dist_actor, DIST_JOIN, 2, argv);
}

void dist_delegate(message_t* msg)
{
	message_t* clone = POOL_ALLOC(message_t);
	memcpy(clone, msg, sizeof(message_t));

	pony_arg_t argv[2];
	argv[0].p = actor_current();
	argv[1].p = clone;

	pony_sendv(dist_actor, DIST_DELEGATE, 2, argv);
}

void dist_finish(bool stats)
{
	pony_arg_t argv[1];
	argv[0].i = stats;

	pony_sendv(dist_actor, DIST_FINISH, 1, argv);
}
