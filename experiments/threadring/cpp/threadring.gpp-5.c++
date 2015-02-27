/*

* The Computer Language Benchmarks Game

* http://benchmarksgame.alioth.debian.org/

* Based on C contribution by Premysl Hruby

* Contributed by The Anh Tran

* added simple custom allocation for completion handlers by Max Skvortsov

*/

/*

   This program will create 503 OS threads.

   Those threads will wait inside boost::asio::io_service.run() queue,

   if there is task in this queue, a thread will be dispatch to process it.

   Each task is a token passing run.



   asio::io_service:

      On Win:      NtIOCompletionPort

      On Linux:   kqueue

*/

#include <sched.h>

#include <pthread.h>

#include <cstdio>


#include <boost/scoped_ptr.hpp>

#include <boost/asio.hpp>

#include <boost/lexical_cast.hpp>

#include <boost/aligned_storage.hpp>

using namespace boost;


enum 
{   
   THREAD_COUNT   = 503,
   STACK_SIZE      = 4*1024
};


// pthread handle

pthread_t         threadid[THREAD_COUNT];
// custom stack space

char            stacks   [THREAD_COUNT][STACK_SIZE];


// Task queue:

scoped_ptr<asio::io_service> iosv;


// how many times remain to pass the token

int token;

// Rule 4: "pass the token from thread to thread N times"

// Any (real OS) thread can pick up the task inside asio::io_service.

// In order to satisfy benchmark rule, this struct encapsulates a "faked" thread 

// that handle each token passing run. 

// The real thread impersonates a faked thread, and passes token to next faked thread.

struct LogicalThread
{
   int my_id;
   int next_id;
   
   void operator()() const;
  
};
LogicalThread   threads[THREAD_COUNT];

//buffer for complection handlers

char handler_buf[1024];

//custom allocator for complection handlers

void* asio_handler_allocate(std::size_t /*size*/, LogicalThread* /*context*/)
{
   return handler_buf;
  //return ::operator new(size);

}

void asio_handler_deallocate(void* /*pointer*/, std::size_t /*size*/,
    LogicalThread* /*context*/)
{
  //::operator delete(pointer);

}


void 
LogicalThread::operator()() const
{
   if (token > 0)
   {
      --token;
      // pass token to next

      iosv->post( threads[next_id] );
   }
   else
      printf("%d\n", my_id);   // done, print who is the final

}


static
void* 
thread_func( void* )
{
   // gather at task queue, waiting for job

   // this run() func will return when queue is empty

   iosv->run();   
   return 0;
}
/*

static

void

SetSingleCPU()

{

   cpu_set_t   aff_set;

   CPU_ZERO(&aff_set);

   

   sched_getaffinity(0, sizeof(aff_set), &aff_set);

   

   int cpu_aff = 0, cpu_count = 0;

   for (int i = 0; i < 64; ++i)

   {

      if (CPU_ISSET(i, &aff_set))

      {

         cpu_aff = i;

         ++cpu_count;

      }

   }



   if (cpu_count > 1)

   {

      CPU_ZERO(&aff_set);

      CPU_SET(cpu_aff, &aff_set);

      sched_setaffinity(0, sizeof(aff_set), &aff_set);

   }

}

*/

int 
main(int argc, char** args)
{
   // set process affinity to 1 cpu,

   // to avoid spreading thread context on all CPUs

//   SetSingleCPU();

   
   // create task queue

   iosv.reset( new asio::io_service );

   // parse input

   if (argc >= 2)
      token = lexical_cast<int>(args[1]);
   else
      token = 1000;


   // Rule 2: "thread 503 should be linked to thread 1, forming an unbroken ring"

   for (int i = 0; i < THREAD_COUNT; i++) 
   {
      threads[i].my_id = i +1;
      threads[i].next_id = i +1;
   }
   threads[THREAD_COUNT -1].next_id = 0;


   // Rule 3: "pass a token to thread 1"

   // post first task to queue, so that task queue is not empty

   // this task will be handled by "thread 0"

   iosv->post( threads[0] );
   

   // Rule 1: "create 503 linked threads (named 1 to 503)"

   // cannot use boost::thread here, b/c creating 503 threads with default 

   // stack size will overflow process's working set

   {
      pthread_attr_t stack_attr;
      pthread_attr_init( &stack_attr );

      for (int i = 0; i < THREAD_COUNT; i++) 
      {
         // manually set stack space & stack size for each thread 

         // to reduce virtual memory cost

         pthread_attr_setstack( &stack_attr, &stacks[i], STACK_SIZE );

         // create thread using customized stack space

         pthread_create( &threadid[i], &stack_attr, &thread_func, 0 );
      }
   }
   
   // waiting

   iosv->run();

   return 0;
}
