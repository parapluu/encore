#include "map.h"
#include "pool.h"
#include "heap.h"
#include "pagemap.h"
#include "actor.h"
#include <string.h>
#include <assert.h>

#define MAP_SIZE 2
#define MAP_REQUEST_INC 256

#define MAP_BLACK 0
#define MAP_ROOT 1
#define MAP_GREY 2
#define MAP_WHITE 3
#define MAP_UNBLOCKED 4

typedef void (*insert_fn)(map_t* map, int index, void* p, int32_t rc);

typedef struct map_entry_t
{
  void* p;

  union
  {
    map_t* map;
    pony_trace_fn f;
    struct map_entry_t* blocked;
  };

  int32_t rc;
  uint32_t mark;
} map_entry_t;

typedef struct map_node_t
{
  map_entry_t e[MAP_SIZE];
  struct map_node_t* next;
} map_node_t;

struct map_t
{
  uint64_t count;
  uint64_t num_nodes;
  map_node_t** nodes;
  map_t* next;
  uint32_t mark;
};

typedef struct cycle_t
{
  map_t* blocked;
  map_t* cycle;
  uint64_t token;
} cycle_t;

typedef struct sweep_t
{
  map_t* keep;
  map_t* release;
} sweep_t;

typedef void (*foreach_fn)(map_t* map, int i, map_entry_t* e, void* arg);
typedef map_t* (*scan_fn)(map_t* map, int i, map_entry_t* e, cycle_t* c);

typedef map_node_t map_node_8[8];
typedef map_node_t map_node_32[8];

static void alloc_nodes(map_t* map, uint64_t size)
{
  map->num_nodes = size;
  size_t len = map->num_nodes * sizeof(map_node_t*);

  switch(size)
  {
    case 8:
      map->nodes = POOL_ALLOC(map_node_8);
      break;

    case 32:
      map->nodes = POOL_ALLOC(map_node_32);
      break;

    default:
      map->nodes = malloc(len);
      break;
  }

  memset(map->nodes, 0, len);
}

static map_t* alloc_size(uint64_t size)
{
  map_t* map = POOL_ALLOC(map_t);
  memset(map, 0, sizeof(map_t));
  alloc_nodes(map, size);

  return map;
}

static uint64_t hash(const void* p)
{
  uint64_t key = (uint64_t)p;

  key = ~key + (key << 21);
  key = key ^ (key >> 24);
  key = (key + (key << 3)) + (key << 8);
  key = key ^ (key >> 14);
  key = (key + (key << 2)) + (key << 4);
  key = key ^ (key >> 28);
  key = key + (key << 31);

  return key;
}

static void foreach(map_t* map, foreach_fn f, void* arg)
{
  if(map == NULL) return;
  map_node_t* node;

  for(int i = 0; i < map->num_nodes; i++)
  {
    node = map->nodes[i];

    while(node != NULL)
    {
      for(int j = 0; j < MAP_SIZE; j++)
      {
        if(node->e[j].p != NULL) f(map, i, &node->e[j], arg);
      }

      node = node->next;
    }
  }
}

static void scan(map_t* map, scan_fn f, cycle_t* c)
{
  map_t* next;
  map_node_t* node;
  map_entry_t* e;

  while(map != NULL)
  {
    for(int i = 0; i < map->num_nodes; i++)
    {
      node = map->nodes[i];

      while(node != NULL)
      {
        for(int j = 0; j < MAP_SIZE; j++)
        {
          e = &node->e[j];
          if(e->p == NULL) continue;

          next = f(map, i, e, c);
          if(next == NULL) continue;

          next->next = map->next;
          map->next = next;
        }

        node = node->next;
      }
    }

    next = map->next;
    map->mark = MAP_BLACK;
    map->next = NULL;
    map = next;
  }
}

static void free_nodes(map_t* map)
{
  map_node_t* node;
  map_node_t* next;

  for(int i = 0; i < map->num_nodes; i++)
  {
    node = map->nodes[i];

    while(node != NULL)
    {
      next = node->next;
      POOL_FREE(map_node_t, node);
      node = next;
    }
  }

  switch(map->num_nodes)
  {
    case 8:
      POOL_FREE(map_node_8, map->nodes);
      break;

    case 32:
      POOL_FREE(map_node_32, map->nodes);
      break;

    default:
      free(map->nodes);
      break;
  }
}

static map_entry_t* insert(map_t* map, void* p);

static void grow_insert(map_t* map, int i, map_entry_t* e, void* arg)
{
  map_entry_t* to = insert(arg, e->p);
  to->map = e->map;
  to->rc = e->rc;
  to->mark = e->mark;
}

static void grow(map_t** map)
{
  map_t* from = *map;
  map_t* to = alloc_size(from->num_nodes << 2);
  to->mark = from->mark;
  *map = to;

  foreach(from, grow_insert, to);
  map_free(from, false);
}

static bool init_map(map_t** map)
{
  if(*map == NULL)
  {
    *map = map_alloc();
  } else if((*map)->count >= (*map)->num_nodes) {
    grow(map);
    return true;
  }

  return false;
}

static map_entry_t* new_entry(map_t* map, int index, void* p, map_entry_t* open)
{
  map->count++;

  if(open == NULL)
  {
    map_node_t* node = POOL_ALLOC(map_node_t);
    memset(node, 0, sizeof(map_node_t));
    node->next = map->nodes[index];
    map->nodes[index] = node;
    open = &node->e[0];
  }

  open->p = p;
  open->f = NULL;
  open->rc = 0;
  open->mark = map->mark;

  return open;
}

static map_entry_t* insert_at(map_t* map, int index, void* p)
{
  map_node_t* node = map->nodes[index];
  map_entry_t* open = NULL;

  if(node != NULL)
  {
    for(int i = 0; i < MAP_SIZE; i++)
    {
      if(node->e[i].p == NULL)
      {
        open = &node->e[i];
        break;
      }
    }
  }

  return new_entry(map, index, p, open);
}

static map_entry_t* insert(map_t* map, void* p)
{
  int index = hash(p) & (map->num_nodes - 1);
  return insert_at(map, index, p);
}

static map_entry_t* find_at(map_t* map, int index, void* p)
{
  map_node_t* node = map->nodes[index];

  while(node != NULL)
  {
    for(int i = 0; i < MAP_SIZE; i++)
    {
      if(node->e[i].p == p)
      {
        return &node->e[i];
      }
    }

    node = node->next;
  }

  return NULL;
}

static map_entry_t* find(map_t* map, void* p)
{
  int index = hash(p) & (map->num_nodes - 1);
  return find_at(map, index, p);
}

static map_entry_t* find_or_insert_at(map_t* map, int index, void* p)
{
  map_node_t* node = map->nodes[index];
  map_entry_t* open = NULL;

  while(node != NULL)
  {
    for(int i = 0; i < MAP_SIZE; i++)
    {
      if(node->e[i].p == p)
      {
        return &node->e[i];
      } else if((open == NULL) && (node->e[i].p == NULL)) {
        open = &node->e[i];
      }
    }

    node = node->next;
  }

  return new_entry(map, index, p, open);
}

static map_entry_t* find_or_insert(map_t* map, void* p)
{
  int index = hash(p) & (map->num_nodes - 1);
  return find_or_insert_at(map, index, p);
}

static bool mark_and_rc(map_t** map, void* p, int32_t rc, map_entry_t** e,
  map_t** to, map_entry_t** to_e)
{
  init_map(map);

  uint32_t mark = (*map)->mark + 1;
  map_entry_t* e2 = find_or_insert(*map, p);
  *e = e2;

  if(e2->mark == mark) return false;
  e2->mark = mark;

  if(-rc >= e2->rc)
  {
    e2->rc += MAP_REQUEST_INC;
    mark_and_rc(to, p, MAP_REQUEST_INC, to_e, NULL, NULL);
  }

  e2->rc += rc;
  return true;
}

static void free_nested(map_t* map, int i, map_entry_t* e, void* arg)
{
  map_free(e->map, false);
}

static void split_marked(map_t* map, int i, map_entry_t* e, void* arg)
{
  sweep_t* sweep = arg;
  map_t* to;

  if(e->mark == map->mark + 1)
  {
    if(sweep->keep == NULL) sweep->keep = alloc_size(map->num_nodes);
    to = sweep->keep;
  } else {
    if(sweep->release == NULL) sweep->release = alloc_size(map->num_nodes);
    to = sweep->release;
  }

  map_entry_t* to_e = insert_at(to, i, e->p);
  to_e->rc = -e->rc;
}

static void sweep_actors(map_t* map, int i, map_entry_t* e, void* arg)
{
  pony_actor_t* actor = e->p;
  pony_arg_t argv[2];

  if(e->mark == (map->mark + 1))
  {
    sweep_t sweep;
    sweep.keep = NULL;
    sweep.release = NULL;
    foreach(e->map, split_marked, &sweep);

    map_free(e->map, false);
    e->map = sweep.keep;

    if(sweep.release == NULL) return;

    argv[0].i = 0;
    argv[1].p = sweep.release;
  } else {
    argv[0].i = -e->rc;
    argv[1].p = e->map;

    e->p = NULL;
    e->rc = 0;
    e->map = NULL;

    assert(map->count > 0);
    map->count--;
  }

  *(bool*)arg = true;
  actor_sendv(actor, ACTORMSG_RC, 2, argv);
}

static void clone_ref(map_t* map, int i, map_entry_t* e, void* arg)
{
  map_t* clone = arg;
  map_entry_t* to_e = insert_at(clone, i, e->p);
  to_e->rc = e->rc;
}

static void trace_gc(map_t* map, int i, map_entry_t* e, void* arg)
{
  pony_trace(&e->p, e->f, 0, PONY_MUTABLE);
}

static void clear_mark(map_t* map, int i, map_entry_t* e, void* arg)
{
  e->mark = map->mark;
}

static void clear_blocked_sub(map_t* map, int i, map_entry_t* e, void* arg)
{
  e->blocked = NULL;
}

static void clear_blocked(map_t* map, int i, map_entry_t* e, void* arg)
{
  foreach(e->map, clear_blocked_sub, NULL);
}

static void receive_rc(map_t* map, int i, map_entry_t* e, void* arg)
{
  map_entry_t* to_e = find_or_insert(arg, e->p);

  assert(to_e->rc >= -e->rc);
  to_e->rc += e->rc;

  if(e->rc == 0)
  {
    e->p = NULL;
    e->f = NULL;

    assert(map->count > 0);
    map->count--;
  }
}

static void send_rc(map_t* map, int i, map_entry_t* e, void* arg)
{
  pony_arg_t argv[2];
  argv[0].i = e->rc;
  argv[1].p = e->map;
  actor_sendv(e->p, ACTORMSG_RC, 2, argv);
}

static map_entry_t* get_blocked(map_t* blocked, map_entry_t* e)
{
  if(e->blocked == NULL) e->blocked = find(blocked, e->p);
  return e->blocked;
}

static map_t* scan_grey(map_t* map, int i, map_entry_t* e, cycle_t* c)
{
  map_entry_t* blocked_e = get_blocked(c->blocked, e);
  if((blocked_e == NULL) || (blocked_e->mark == MAP_UNBLOCKED)) return NULL;

  blocked_e->rc -= e->rc;
  if(blocked_e->mark == MAP_GREY) return NULL;

  blocked_e->mark = MAP_GREY;
  return blocked_e->map;
}

static map_t* scan_black(map_t* map, int i, map_entry_t* e, cycle_t* c)
{
  map_entry_t* blocked_e = get_blocked(c->blocked, e);
  if((blocked_e == NULL) || (blocked_e->mark == MAP_UNBLOCKED)) return NULL;

  if(map->mark == MAP_GREY) blocked_e->rc += e->rc;

  if((blocked_e->rc != 0) && (blocked_e->mark != MAP_BLACK))
  {
    blocked_e->mark = MAP_BLACK;
    if(blocked_e->map != NULL) blocked_e->map->mark = MAP_GREY;
    return blocked_e->map;
  } else if((blocked_e->rc == 0) && (blocked_e->mark == MAP_GREY)) {
    blocked_e->mark = MAP_WHITE;
    return blocked_e->map;
  }

  return NULL;
}

static map_t* scan_white(map_t* map, int i, map_entry_t* e, cycle_t* c)
{
  map_entry_t* blocked_e = get_blocked(c->blocked, e);
  if((blocked_e == NULL) || (blocked_e->mark == MAP_UNBLOCKED)) return NULL;

  blocked_e->rc += e->rc;
  if(blocked_e->mark != MAP_WHITE) return NULL;

  init_map(&c->cycle);
  insert(c->cycle, blocked_e->p);
  pony_sendi(blocked_e->p, ACTORMSG_CONF, c->token);

  blocked_e->mark = MAP_BLACK;
  return blocked_e->map;
}

static map_t* detect_cycle(map_t* map, int index, map_entry_t* e, uint64_t t)
{
  cycle_t c;
  c.blocked= map;

  e->mark = MAP_GREY;
  scan(e->map, scan_grey, &c);

  if(e->rc != 0)
  {
    e->mark = MAP_BLACK;
    if(e->map != NULL) e->map->mark = MAP_GREY;
  } else {
    e->mark = MAP_WHITE;
  }

  scan(e->map, scan_black, &c);

  if(e->mark != MAP_WHITE)
  {
    assert(e->rc > 0);
    assert(e->mark == MAP_BLACK);
    return NULL;
  }

  assert(e->rc == 0);
  c.cycle = map_alloc();
  c.token = t;

  insert(c.cycle, e->p);
  pony_sendi(e->p, ACTORMSG_CONF, t);

  e->mark = MAP_BLACK;
  scan(e->map, scan_white, &c);

  return c.cycle;
}

static void remove_cycle(map_t* map, int i, map_entry_t* e, void* arg)
{
  map_t* cycle = arg;
  map_entry_t* cycle_e = find(cycle, e->p);
  if(cycle_e == NULL) return;

  map_free(e->map, false);
  e->p = NULL;
  e->rc = 0;
  e->map = NULL;
  e->mark = map->mark;
}

static void collect_cycle(map_t* map, int i, map_entry_t* e, void* arg)
{
  actor_destroy(e->p, map);
}

map_t* map_alloc()
{
  return alloc_size(8);
}

uint64_t map_size(map_t* map)
{
  return map->count;
}

void map_free(map_t* map, bool nested)
{
  if(map == NULL) return;
  if(nested) foreach(map, free_nested, NULL);

  free_nodes(map);
  POOL_FREE(map_t, map);
}

void map_createactor(map_t** map, pony_actor_t* actor)
{
  init_map(map);
  map_entry_t* e = insert(*map, actor);
  e->rc = MAP_REQUEST_INC;
}

void map_receiveactor(map_t** map, pony_actor_t* actor)
{
  map_entry_t* e;
  mark_and_rc(map, actor, 1, &e, NULL, NULL);
}

bool map_receiveobject(map_t** map, pony_actor_t* owner, void* object)
{
  map_entry_t* e;
  mark_and_rc(map, owner, 1, &e, NULL, NULL);
  return mark_and_rc(&e->map, object, 1, &e, NULL, NULL);
}

void map_sendactor(map_t** map, map_t** to, pony_actor_t* actor)
{
  map_entry_t* e;
  map_entry_t* to_e;
  mark_and_rc(map, actor, -1, &e, to, &to_e);
}

bool map_sendobject(map_t** map, map_t** to, pony_actor_t* owner, void* object)
{
  map_entry_t* e;
  map_entry_t* to_e;
  map_entry_t* junk;
  mark_and_rc(map, owner, -1, &e, to, &to_e);
  return mark_and_rc(&e->map, object, -1, &junk, &to_e->map, &junk);
}

void map_markactor(map_t** map, map_t** to, pony_actor_t* actor)
{
  map_entry_t* e;
  map_entry_t* to_e;
  mark_and_rc(map, actor, 0, &e, to, &to_e);
}

bool map_markobject(map_t** map, map_t** to, pony_actor_t* owner, void* object)
{
  map_entry_t* e;
  map_entry_t* to_e;
  map_entry_t* junk;
  mark_and_rc(map, owner, 0, &e, to, &to_e);
  return mark_and_rc(&e->map, object, 0, &junk, &to_e->map, &junk);
}

bool map_sweep(map_t* map)
{
  bool res = false;
  foreach(map, sweep_actors, &res);
  return res;
}

map_t* map_clonerefs(map_t* map)
{
  if((map == NULL) || (map->count == 0)) return NULL;

  map_t* clone = alloc_size(map->num_nodes);
  foreach(map, clone_ref, clone);

  return clone;
}

bool map_receivelocal(map_t** map, void* object)
{
  map_entry_t* e;
  return mark_and_rc(map, object, 0, &e, NULL, NULL);
}

bool map_sendlocal(map_t** map, void* object, pony_trace_fn f)
{
  map_entry_t* e;

  if(mark_and_rc(map, object, 1, &e, NULL, NULL))
  {
    e->f = f;
    return true;
  }

  return false;
}

void map_gc(map_t* map)
{
  foreach(map, trace_gc, NULL);
}

void map_receiverc(map_t** map, map_t* objects)
{
  if(objects == NULL) return;
  init_map(map);

  foreach(objects, receive_rc, *map);
  map_free(objects, false);
}

void map_sendrc(map_t** map)
{
  if(*map == NULL) return;

  foreach(*map, send_rc, NULL);
  map_free(*map, false);
  *map = NULL;
}

bool map_contains(map_t* map, void* p)
{
  int index = hash(p) & (map->num_nodes - 1);
  map_node_t* node = map->nodes[index];

  while(node != NULL)
  {
    for(int i = 0; i < MAP_SIZE; i++)
    {
      if(node->e[i].p == p) return true;
    }

    node = node->next;
  }

  return false;
}

void map_block(map_t** map, pony_actor_t* actor, uint32_t rc, map_t* ref)
{
  if(init_map(map)) foreach(*map, clear_blocked, NULL);

  map_entry_t* e = find_or_insert(*map, actor);
  e->rc = rc;
  e->mark = MAP_ROOT;

  if(ref != NULL)
  {
    map_free(e->map, false);
    e->map = ref;
  }
}

void map_unblock(map_t* map, pony_actor_t* actor)
{
  map_entry_t* e = find(map, actor);
  assert(e != NULL);
  assert(e->mark != MAP_UNBLOCKED);

  e->rc = 0;
  e->mark = MAP_UNBLOCKED;
}

void map_updaterc(map_t* map, pony_actor_t* actor, uint32_t rc)
{
  map_entry_t* e = find(map, actor);
  e->rc = rc;

  if(rc == 0)
  {
    map_free(e->map, false);
    e->p = NULL;
    e->map = NULL;
    e->mark = MAP_BLACK;

    assert(map->count > 0);
    map->count--;
  } else {
    e->mark = MAP_ROOT;
  }
}

bool map_detectcycles(map_t* map, map_cycle_fn f, void* arg, uint64_t t)
{
  if(map == NULL) return false;

  map_t* cycle;
  map_node_t* node;
  bool result = false;

  for(int i = 0; i < map->num_nodes; i++)
  {
    node = map->nodes[i];

    while(node != NULL)
    {
      for(int j = 0; j < MAP_SIZE; j++)
      {
        if(node->e[j].mark == MAP_ROOT)
        {
          cycle = detect_cycle(map, i, &node->e[j], t);

          if(cycle != NULL)
          {
            t++;
            f(arg, cycle);
            result = true;
          }
        }
      }

      node = node->next;
    }
  }

  return result;
}

void map_collectcycle(map_t* map)
{
  foreach(map, collect_cycle, NULL);
}

void map_removecycle(map_t* map, map_t* cycle)
{
  if(cycle == NULL) return;
  foreach(map, remove_cycle, cycle);
}

void map_clearmarks(map_t* map)
{
  if(map == NULL) return;
  map->mark++;
  if(map->mark == 0) foreach(map, clear_mark, NULL);
}
