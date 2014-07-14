/**
   @file set.c
*/
#include "set.h"
#include <assert.h>

// Controls whether malloc of pony_alloc is to be used
#define USE_PONY_ALLOC 

#ifdef USE_PONY_ALLOC
  #include <pony/pony.h>
#else
  #include <stdlib.h>
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

set_t *mk_set(void){
#ifdef USE_PONY_ALLOC
  set_t *set = pony_alloc(sizeof(set_t));
#else
  set_t *set = malloc(sizeof(set_t));
#endif
  if(set)
    set->root = NULL;
  return set;
}

static struct node *node_mk(void *elem){
#ifdef USE_PONY_ALLOC
  struct node *node = pony_alloc(sizeof(struct node));
#else
  struct node *node = malloc(sizeof(struct node));
#endif
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
#ifdef USE_PONY_ALLOC
#else
  free(tmp);
#endif
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

    newSet = mk_set();
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
#ifdef USE_PONY_ALLOC
#else
    free(node);
#endif
  }
}

void set_destroy(set_t *set){
  if(set){
    node_destroy(set->root);
#ifdef USE_PONY_ALLOC
#else
    free(set);
#endif
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
    return node_map(set->root, mk_set(), f);
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
