BEGIN
{
  successful_steals = 0;
  unsuccessful_steals = 0;
  actors = 0;
}

pony$target:::actor-alloc
{
  actors++;
}
pony$target:::work-steal-successful
{
  printf("%d stole from %d", arg1, arg2);
  successful_steals++;
}

pony$target:::work-steal-failure
{
  unsuccessful_steals++;
}

END
{
  printf("\nSuccessful steals: %d\n", successful_steals);
  printf("Failed steals: %d\n", unsuccessful_steals);
  printf("Actors created: %d\n", actors);
}