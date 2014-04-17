

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

class Other {
  int count;

  void init() {
    count = 10
  }
  void main() {
     while (count > 0) {
       print count;
       count--;
     }
     print "Hello Ponyworld!"
  }
}


*/

#include <pony/pony.h>
#include <stdlib.h>
#include <stdio.h>


typedef struct main_t
{
} main_t;


typedef struct other_t
{
	int count;
} other_t;

enum
{
  MSG_INIT,
  MSG_WORK,
};


static void trace_main(void* p)
{
  main_t* d = p;
}


static void trace_other(void* p)
{
  other_t* d = p;
  pony_trace32(&d->count);
}


static pony_msg_t m_other_init = {0, {{NULL, 0, PONY_PRIMITIVE}}};
static pony_msg_t m_other_work = {2, {{NULL, 0, PONY_PRIMITIVE}}};

static pony_msg_t* message_type_other(uint64_t id)
{
  switch(id)
  {
    case MSG_INIT: return &m_other_init;
    case MSG_WORK: return &m_other_work;
  }

  return NULL;
}

static pony_msg_t* message_type_main(uint64_t id)
{
  return NULL;
}


static void dispatch_main(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv);
static void dispatch_other(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv);


static pony_actor_type_t main_type =
{
  1,
  {trace_main, sizeof(main_t), PONY_ACTOR},
  message_type_main,
  dispatch_main
};


static pony_actor_type_t other_type =
{
  2,
  {trace_other, sizeof(other_t), PONY_ACTOR},
  message_type_other,
  dispatch_other
};

static void Main_main(main_t* this) {
	pony_actor_t* other = pony_create(&other_type);

    pony_send(other, MSG_INIT);
    pony_send(other, MSG_WORK);
}

static void Other_init(other_t* this) {
	this->count = 10;
}

static void Other_work(other_t* this) {
	while (this->count > 0) {
		printf("%d\n", this->count);
		this->count--;
	}
	printf("%s\n", "Hello Ponyworld!");
}


static void dispatch_main(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv)
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


static void dispatch_other(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv)
{
  other_t* d = p;

  switch(id)
  {
    case MSG_INIT:
    {
		d = pony_alloc(sizeof(other_t));  
		pony_set(d);
		Other_init(d);
		break;
    }

	case MSG_WORK: {
		Other_work(d);
		break;
	}
  }
}

int main(int argc, char** argv)
{
  return pony_start(argc, argv, pony_create(&main_type));
}