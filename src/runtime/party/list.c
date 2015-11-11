#include "list.h"
#include <mem/pool.h>

typedef struct list_t
{
  encore_arg_t data;
  struct list_t* next;
} list_t;

list_t* list_push(list_t* list, encore_arg_t data)
{
  list_t* l = (list_t*)POOL_ALLOC(list_t);
  l->data = data;
  l->next = list;

  return l;
}

encore_arg_t list_data(list_t* list)
{
  return list->data;
}

size_t list_length(list_t* list)
{
  size_t len = 0;

  while(list != NULL)
  {
    len++;
    list = list->next;
  }

  return len;
}

list_t* list_index(list_t* list, ssize_t index)
{
  if(index < 0)
    index = list_length(list) + index;

  for(int i = 0; (list != NULL) && (i < index); i++)
    list = list->next;

  return list;
}
