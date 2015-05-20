/**
   @file set.c
*/
#include "set.h"
#include <assert.h>

// Controls whether malloc of pony_alloc is to be used
#define USE_PONY_ALLOC 1

#ifdef USE_PONY_ALLOC
  #include <pony.h>
  #include "../libponyrt/actor/actor.h"
  // Tobias: this is a hack to make top-level functions work as they are implemented as closures right now
  // This hack creates a small memory leak which we can safely ignore until top-level functions change.
  #define ALLOC(size) (actor_current() ? pony_alloc(size) : malloc(size))
  #define FREE(ptr)
#else
  #include <stdlib.h> 
  #define ALLOC(size) malloc(size)
  #define FREE(ptr) free(ptr)
#endif

#define NOP 0
#define TRUE 1
#define FALSE 0

struct node{
  void *elem;
  struct node* left;
  struct node* right;
};

struct set{
  struct node *root;
};

set_t *set_mk(void){
  set_t *set = ALLOC(sizeof(set_t));
  if(set)
    set->root = NULL;
  return set;
}

static struct node *node_mk(void *elem){
  struct node *node = ALLOC(sizeof(struct node));
  if(node){
    node->elem = elem;
    node->left = NULL;
    node->right = NULL;
  }
  return node;
}

bool set_add(set_t *set, void *elem){
  if(set){
    if(!set->root){
      set->root = node_mk(elem);
      return TRUE;
    }
    struct node **cursor = &(set->root);
    while(*cursor != NULL && (*cursor)->elem != elem){
      cursor = (*cursor)->elem > elem? &((*cursor)->left): &((*cursor)->right);
    }
    if(*cursor == NULL){
      *cursor = node_mk(elem);
      return TRUE;
    }else // (*cursor)->elem == elem  -- the element was already in the list
      return FALSE;
  }
  return FALSE;
}

bool set_elem(set_t *set, void *elem){
  if(set){
    struct node *cursor = set->root;
    while(cursor != NULL){
      if(cursor->elem > elem)
        cursor = cursor->left;
      else if(cursor->elem < elem)
        cursor = cursor->right;
      else // cursor->elem == elem
        return TRUE;
    }
  }
  return FALSE;
}

static struct node *extractMin(struct node** root){
  struct node **cursor = root;
  while((*cursor)->left)
    cursor = &((*cursor)->left);
  struct node *tmp = *cursor;
  *cursor = (*cursor)->right;
  return tmp;
}

static void removeNode(struct node **nodePtr){
  assert(*nodePtr);
  struct node *tmp = *nodePtr;
  if(!tmp->right)
    *nodePtr = (*nodePtr)->left;
  else{
    struct node *newRoot = extractMin(&(tmp->right));
    newRoot->left = (*nodePtr)->left;
    newRoot->right = (*nodePtr)->right;
    *nodePtr = newRoot;
  }
  FREE(tmp);
}

bool set_remove(set_t *set, void *elem){
  if(!set || !(set->root))
    return FALSE;
  struct node **cursor = &(set->root);
  while(*cursor != NULL && (*cursor)->elem != elem){
    cursor = (*cursor)->elem > elem? &((*cursor)->left): &((*cursor)->right);
  }
  if(*cursor == NULL)
    return FALSE;
  else{ // the element was found and should be removed
    removeNode(cursor);
    return TRUE;
  }
}

static bool node_subset(struct node *node, set_t *set){
  if(!node)
    return TRUE;
  return set_elem(set, node->elem) && 
    node_subset(node->left, set) && 
    node_subset(node->right, set);

}

bool set_subset(set_t *sub, set_t *super){
  if(sub)
    return node_subset(sub->root, super);
  else
    return FALSE;
}

bool set_eq(set_t *set, set_t *other){
  return set_subset(set, other) && set_subset(other, set);
}

static void cloneNode(struct node *from, struct node **to){
  if(from){
    *to = node_mk(from->elem);
    cloneNode(from->left, &((*to)->left));
    cloneNode(from->right, &((*to)->right));
  }
}

set_t *set_clone(set_t *set){
  set_t *newSet = NULL;
  if(set)

    newSet = set_mk();
  if(newSet)
    cloneNode(set->root, &(newSet->root));
  return newSet;
}

static void printNode(struct node *node, printer_fnc printer){
  if(node){
    if(node->left)
      printNode(node->left, printer);
    printer(node->elem);
    if(node->right)
      printNode(node->right, printer);
  }
}

void set_print(set_t *set, printer_fnc printer){
  if(set)
    printNode(set->root, printer);
}

static void node_destroy(struct node* node){
  if(node){
    node_destroy(node->left);
    node_destroy(node->right);
    FREE(node);
  }
}

void set_destroy(set_t *set){
  if(set){
    node_destroy(set->root);
    FREE(set);
  }
}

static set_t *node_map(struct node *node, set_t *result, map_fnc f){
  if(node){
    set_add(result, f(node->elem));
    node_map(node->left, result, f);
    node_map(node->right, result, f);
  }
  return result;
}

set_t *set_map(set_t *set, map_fnc f){
  if(set)
    return node_map(set->root, set_mk(), f);
  else
    return NULL;
}

static void node_forall(struct node *node, forall_fnc f, void *arg){
  if(node){
    f(node->elem, arg);
    node_forall(node->left, f, arg);
    node_forall(node->right, f, arg);
  }
}

void set_forall(set_t *set, forall_fnc f, void *arg){
  if(set)
    node_forall(set->root, f, arg);
}

static void node_forall_closure(struct node *node, closure_t *c){
  if(node){
    value_t args[1] = {ptr_to_val(node->elem)};
    closure_call(c, args);
    node_forall_closure(node->left, c);
    node_forall_closure(node->right, c);
  }
}

void set_forall_closure(set_t *set, closure_t *c){
  if(set)
    node_forall_closure(set->root, c);
}

static void *node_reduce(struct node *node, reduce_fnc f, void *acc){
  if(!node)
    return acc;
  else{
    void *left = node_reduce(node->left, f, acc);
    void *mid  = f(node->elem, left);
    return node_reduce(node->right, f, mid);
  }
}

void *set_reduce(set_t *set, reduce_fnc f, void *init){
  if(set)
    return node_reduce(set->root, f, init);
  else
    return NULL;
}
