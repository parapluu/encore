/**
   @file set.c
*/
#include "set.h"
#include <stdlib.h>
#include <assert.h>

#define NOP 0
#define TRUE 1
#define FALSE 0
typedef int bool;

struct node{
  void *elem;
  struct node* left;
  struct node* right;
};

struct set{
  struct node *root;
};

Set set_new(void){
  struct set *set = malloc(sizeof(struct set));
  if(set)
    set->root = NULL;
  return set;
}

static struct node *node_mk(void *elem){
  struct node *node = malloc(sizeof(struct node));
  if(node){
    node->elem = elem;
    node->left = NULL;
    node->right = NULL;
  }
  return node;
}

bool set_add(Set set, void *elem){
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

bool set_elem(Set set, void *elem){
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
  free(tmp);
}

bool set_remove(Set set, void *elem){
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

static bool node_subset(struct node *node, Set set){
  if(!node)
    return TRUE;
  return set_elem(set, node->elem) && 
    node_subset(node->left, set) && 
    node_subset(node->right, set);

}

bool set_subset(Set sub, Set super){
  if(sub)
    return node_subset(sub->root, super);
  else
    return FALSE;
}

bool set_eq(Set set, Set other){
  return set_subset(set, other) && set_subset(other, set);
}

static void cloneNode(struct node *from, struct node **to){
  if(from){
    *to = node_mk(from->elem);
    cloneNode(from->left, &((*to)->left));
    cloneNode(from->right, &((*to)->right));
  }
}

Set set_clone(Set set){
  Set newSet = NULL;
  if(set)
    newSet = set_new();
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

void set_print(Set set, printer_fnc printer){
  if(set)
    printNode(set->root, printer);
}

static void node_destroy(struct node* node){
  if(node){
    node_destroy(node->left);
    node_destroy(node->right);
    free(node);
  }
}

void set_destroy(Set set){
  if(set){
    node_destroy(set->root);
    free(set);
  }
}

static Set node_map(struct node *node, Set result, map_fnc f){
  if(node){
    set_add(result, f(node->elem));
    node_map(node->left, result, f);
    node_map(node->right, result, f);
  }
  return result;
}

Set set_map(Set set, map_fnc f){
  if(set)
    return node_map(set->root, set_new(), f);
  else
    return NULL;
}

static void node_forall(struct node *node, map_fnc f){
  if(node){
    f(node->elem);
    node_forall(node->left, f);
    node_forall(node->right, f);
  }
}

void set_forall(Set set, map_fnc f){
  if(set)
    node_forall(set->root, f);
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

void *set_reduce(Set set, reduce_fnc f, void *init){
  if(set)
    return node_reduce(set->root, f, init);
  else
    return NULL;
}