#ifndef list_party_h
#define list_party_h

#include <encore.h>

typedef struct list_t list_t;


encore_arg_t list_data(list_t* list);

list_t* list_push(list_t* list, encore_arg_t data);

size_t list_length(list_t* list);

list_t* list_index(list_t* list, ssize_t index);

#endif
