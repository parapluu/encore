#include "set.h"
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

void *string_to_upper(void *elem){
  char *cursor = (char *) elem;
  while(*cursor != '\0'){
    *(cursor) = toupper(*cursor);
    cursor++;
  }
  return NULL;
}

void print_string(void *elem){
  printf("%s\n", (char*)elem);
}

void print_int(void *elem){
  printf("%d\n", (int)elem);
}

void *bump(void *elem){
  return (void*) ((long)(((int) elem)*10));
}

void *plus(void *elem, void *acc){
  return (void*) ((long)((int) elem) + ((int) acc));
}

int main()
{
  Set s = set_new();
  set_add(s, (void*)3);
  set_add(s, (void*)5);
  set_add(s, (void*)4);
  set_add(s, (void*)1);
  set_add(s, (void*)2);

  printf("s contains the following elements:\n");
  set_print(s, print_int);

  printf("Let's multiply each value by ten:\n");
  Set newSet = set_map(s, bump);
  set_print(newSet, print_int);

  void *result = set_reduce(newSet, plus, (void*)0);
  printf("Reducing this set with (+) yields %d\n", (int)result);

  printf("Okay, now back to s. We can clone it into s2.\n");
  Set s2 = set_clone(s);
  if(set_eq(s, s2))
    printf("s and s2 are now the same\n");
  else
    printf("SOMETHING HAS GONE TERRIBLY WRONG!\n");
  
  set_remove(s2, (void*)3);
  printf("If we remove 3 from s2 ");
  if(!set_eq(s, s2))
    printf("they are not the same anymore.\n");
  else
    printf("SOMETHING GOES TERRIBLY WRONG!\n");
  
  printf("However, ");
  if(set_subset(s2, s))
    printf("s2 is now a subset of s.\n");
  else
    printf("SOMETHING GOES TERRIBLY WRONG!\n");

  printf("Let's also try an in-place map.\n");
  char *word1 = malloc(10);
  char *word2 = malloc(10);
  char *word3 = malloc(10);
  strcpy(word1, "foo");
  strcpy(word2, "bar");
  strcpy(word3, "baz");

  Set wordSet = set_new();
  set_add(wordSet, word1);
  set_add(wordSet, word2);
  set_add(wordSet, word3);
  
  printf("Here's a new set containing words:\n");
  set_print(wordSet, print_string);

  printf("Here is the same set after a mapping toUpper:\n");
  set_destroy(set_map(wordSet, string_to_upper));
  set_print(wordSet, print_string);

  printf("Finally, we remove all four sets from memory.\n");
  set_destroy(s);
  set_destroy(newSet);
  set_destroy(s2);
  set_destroy(wordSet);

  free(word1);
  free(word2);
  free(word3);
  return 0;
}
