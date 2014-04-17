

/*  Approximate source code 

class Main {

  int count; 

  void main {
     count = 10;

     while (count > 0) {
       print count;
       count--;
     }
  } 
}


*/

#include <pony/pony.h>
#include <stdlib.h>
#include <stdio.h>


typedef struct main_t
{
	int count;
} main_t;


static void trace(void* p)
{
  main_t* d = p;
  pony_trace32(&d->count);
}

static pony_msg_t* message_type(uint64_t id)
{
  return NULL;
}

static void dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv);

static pony_actor_type_t type =
{
  1,
  {trace, sizeof(main_t), PONY_ACTOR},
  message_type,
  dispatch
};

static void Main_main(main_t* this) {
	this->count = 10;
	while (this->count > 0) {
		printf("%d\n", this->count);
		this->count--;
	}
}

static void dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv)
{
  main_t* d = p;

  switch(id)
  {
    case PONY_MAIN:
    {
		d = pony_alloc(sizeof(main_t));  // does this and the following belong here, or in Main_main
		pony_set(d);
		Main_main(d);
    }
  }
}

int main(int argc, char** argv)
{
  return pony_start(argc, argv, pony_create(&type));
}