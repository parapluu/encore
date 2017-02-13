#include "set.h"
#include <stddef.h>
#include <stdio.h>
#include <assert.h>
#include "list.h"
#include "structure.h"

typedef struct node_s {
  struct node_s *left;
  struct node_s *right;
  value_t data;
} node_s;

struct set_s {
  node_s *root;
  size_t  size;
  closure_t *cmp;
};

set_s* party_new_set(pony_ctx_t **ctx, closure_t *cmp){
  assert(cmp != NULL);
  set_s *s = encore_alloc(*ctx, sizeof* s);
  s->size = 0;
  s->cmp = cmp;
  return s;
}

static inline set_s* party_node_add(pony_ctx_t *ctx, set_s *s, value_t data){
  node_s *node = s->root;
  if (s->root == NULL) {
    node = encore_alloc(ctx, sizeof* node);
    node->data = data;
    s->root = node;
    return s;
  }

  closure_t *cmp = s->cmp;
  node_s *prev = NULL;
  while(true){
    if (node != NULL) {
      value_t args[] = {data, node->data};
      intptr_t i = closure_call(&ctx, cmp, args).i;
      if (i < 0) {
        prev = node;
        node = node->left;
      } else if (i > 0){
        prev = node;
        node = node->right;
      } else {
        return s;
      }
    } else {
      node_s *new = encore_alloc(ctx, sizeof* new);
      new->data = data;
      ++s->size;

      value_t args[] = {data, prev->data};
      intptr_t i = closure_call(&ctx, cmp, args).i;
      if (i < 0) {
        prev->left = new;
      } else {
        prev->right = new;
      }
      return s;
    }
  }
  exit(-1);
}

// TODO: this is an unBalance TreeSet. create an AVL tree
set_s* party_set_add(pony_ctx_t **ctx, set_s* s, value_t data) {
  assert(s != NULL);
  return party_node_add(*ctx, s, data);
}

par_t* party_set_to_party(pony_ctx_t **ctx, set_s* s, pony_type_t *type) {
  list_t *l = NULL;

  par_t *p = new_par_empty(ctx, type);
  if (s == NULL){
    return p;
  }
  node_s *current = s->root;
  while(current) {
    p = new_par_p(ctx, p, new_par_v(ctx, current->data, type), type);
    if ((current->left != NULL) && (current->right != NULL)) {
      l = list_push(l, (value_t){.p = current->right });
      current = current->left;
    } else if (current->left) {
      current = current->left;
    } else if (current->right) {
      current = current->right;
    } else {
      l = list_pop(l, (value_t*)&current);
    }
  }
  return p;
}


set_s* party_to_set(pony_ctx_t **ctx,
                    par_t *p,
                    closure_t *cmp,
                    pony_type_t *type){
  set_s *set = party_new_set(ctx, cmp);
  list_t *l = NULL;

  par_t *current = p;
  while(current){
    PTAG tag = party_tag(current);
    switch (tag) {
    case EMPTY_PAR: {
      l = list_pop(l, (value_t*)&current);
      break;
    }
    case VALUE_PAR: {
      set = party_set_add(ctx, set, party_get_v(current));
      l = list_pop(l, (value_t*)&current);
      break;
    }
    case FUTURE_PAR: {
      future_t *f = party_get_fut(current);
      set = party_set_add(ctx, set, future_get_actor(ctx, f));
      l = list_pop(l, (value_t*)&current);
      break;
    }
    case PAR_PAR: {
      par_t *right = party_get_parright(current);
      l = list_push(l, (value_t){.p = right } );
      current = party_get_parleft(current);
      break;
    }
    case FUTUREPAR_PAR: {
      // NOTE: if Fut Par t are not fulfilled yet, the actor will use extract
      //       (which is a blocking operation) to get the innner ParT
      // OPTIMISATION: optimise this case if Fut (Par t) are heavily used,
      //               e.g. no need to compute the array because it won't be used!
      party_extract(ctx, current, type);

      // At this point, the whole ParT has been realised.
      current = future_get_actor(ctx, party_get_futpar(current)).p;
      break;
    }
    case ARRAY_PAR: {
      // OPTIMISATION: optimise this case if Par [a] are heavily used
      array_t *a = party_extract(ctx, current, type);
      size_t size = array_size(a);
      for (size_t i = 0; i < size; ++i) {
        party_set_add(ctx, set, array_get(a, i));
      }
      l = list_pop(l, (value_t*)&current);
      break;
    }
    default: exit(-1);
    }
  }
  return set;
}

bool party_set_lookup(pony_ctx_t **ctx, set_s *s, value_t data){
  assert(s != NULL);
  assert(s->cmp != NULL);

  node_s *node = s->root;
  closure_t *cmp = s->cmp;
  while(true){
    if (node != NULL) {
      value_t args[] = {data, node->data};
      intptr_t i = closure_call(ctx, cmp, args).i;
      if (i < 0) {
        node = node->left;
      } else if (i > 0){
        node = node->right;
      } else {
        return true;
      }
    } else {
      return false;
    }
  }
  exit(-1);
}


set_s* party_set_intersection(pony_ctx_t **ctx, set_s* sl, set_s* sr){
  assert(sl != NULL);
  assert(sr != NULL);
  assert(sl->cmp != NULL);
  assert(sr->cmp != NULL);

  closure_t *cmp = sl->cmp;
  set_s *new = party_new_set(ctx, cmp);
  list_t *l = NULL;
  value_t data;

  // Take the smallest Set to iterate over and lookup in the bigger set
  node_s *current = sl->size < sr->size ? sl->root : sr->root;
  set_s *lookup_set = sl->size < sr->size ? sr : sl;
  while(true){
    if(current != NULL) {
      data = current->data;
      if (party_set_lookup(ctx, lookup_set, data)) {
        new = party_set_add(ctx, new, data);
      }
      l = list_push(l, (value_t){ .p = current->right });
      current = current->left;
    } else {
      l = list_pop(l, (value_t*)(&current));
      if (!current) {
        return new;
      }
    }
  }
}
