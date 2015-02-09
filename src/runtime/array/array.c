#include "array.h"

struct array_t{
  size_t size;
  pony_type_t *type;
  encore_arg_t *array;
};

pony_type_t array_type = 
  {
    ID_ARRAY, 
    sizeof(struct array_t), 
    array_trace, 
    NULL, 
    NULL,
    NULL,
    NULL
  };

void array_trace(void *p){
  struct array_t *header = p;
  pony_trace(header->array);
  if(header->type == ENCORE_ACTIVE){
    for(int i = 0; i < header->size; i++){
      pony_traceactor(header->array[i].p);
    }
  } else if(header->type != ENCORE_PRIMITIVE){
    for(int i = 0; i < header->size; i++){
      pony_traceobject(header->array[i].p, header->type->trace);
    }
  }
}

array_t *array_mk(size_t size, pony_type_t *type){
  struct array_t *header = pony_alloc(sizeof(struct array_t));
  encore_arg_t *array = pony_alloc(sizeof(encore_arg_t) * size);
  header->size = size;
  header->type = type;
  header->array = array;
  return header;
}

inline size_t array_size(array_t *a){
  return ((struct array_t *)a)->size;
}

inline encore_arg_t array_get(array_t *a, size_t i){
  return ((struct array_t *)a)->array[i];
}

inline void array_set(array_t *a, size_t i, encore_arg_t element){
  ((struct array_t *)a)->array[i] = element;
}