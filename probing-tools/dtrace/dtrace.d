#pragma D option quiet

BEGIN {
<<<<<<< HEAD
	total_steals = 0;
	total_failed_steals = 0;
	total_steal_attempts = 0;
	total_schedulings = 0;
}


=======
	printf("Dtrace started!");
}

END {
	printf("Dtrace finished!");
}
>>>>>>> 8a72325950f12b752f7f408fb9439a5cc5e393a6

pony$target:::actor-alloc { }
pony$target:::actor-msg-send { }
pony$target:::actor-msg-run { }
<<<<<<< HEAD

// arg[0] is scheduler, arg[1] is actor
pony$target:::actor-scheduled
{
	total_schedulings++;
	@schedulers_for_actor[args[0], args[1]] = count();
}

=======
pony$target:::actor-scheduled { }
>>>>>>> 8a72325950f12b752f7f408fb9439a5cc5e393a6
pony$target:::actor-descheduled { }
pony$target:::cpu-nanosleep { }
pony$target:::gc-end {}
pony$target:::gc-send-end {}
pony$target:::gc-send-start {}
pony$target:::gc-recv-end {}
pony$target:::gc-recv-start {}
pony$target:::gc-start {}
pony$target:::gc-threshold {}
pony$target:::heap-alloc {}
pony$target:::rt-init {}
pony$target:::rt-start {}
pony$target:::rt-end {}
<<<<<<< HEAD

pony$target:::work-steal-successful
{
	total_steals++;
	total_steal_attempts++;
  @steal_success_count[args[0]] = count();
}
pony$target:::work-steal-failure
{
	total_failed_steals++;
	total_steal_attempts++;
  @steal_fail_count[args[0]] = count();
}
=======
pony$target:::work-steal-successful {}
pony$target:::work-steal-failure {}
>>>>>>> 8a72325950f12b752f7f408fb9439a5cc5e393a6

encore$target:::closure-create {}
encore$target:::future-block {}
encore$target:::future-chaining {}
encore$target:::future-create {}
encore$target:::future-destroy {}
encore$target:::future-fulfil-start {}
encore$target:::future-fulfil-end {}
encore$target:::future-get {}
encore$target:::future-unblock {}
encore$target:::field-access {}
encore$target:::field-write {}
// encore$target:::method-call {}
encore$target:::method-entry {}
encore$target:::method-exit {}
encore$target:::function-call {}
encore$target:::function-entry {}
encore$target:::function-exit {}
// encore$target:::closure-call {}
// encore$target:::closure-entry {}
// encore$target:::closure-exit {}
<<<<<<< HEAD

END {
	printf("Total steal attempts:\t\t %d\n", total_steal_attempts);
	printf("Total successful steals:\t %d\n", total_steals);
	printf("Total failed steal attempts:\t %d\n", total_failed_steals);
	printf("Total schedulings: \t\t%d\n", total_schedulings);

	printf("\n");
	printf("Actors scheduled by Schedulers\n");
	printf("Scheduler ID\tActor ID\tCount\n");
	printa("%d\t%d%@8u\n", @schedulers_for_actor);

	printf("\n");
	printf("Successive steals\n");
	printf("Scheduler ID\tCount\n");
	printa("%d%@8u\n", @steal_success_count);

	printf("\n");
	printf("Failed steals\n");
	printf("Scheduler ID\tCount\n");
	printa("%d%@8u\n", @steal_fail_count);
}
=======
>>>>>>> 8a72325950f12b752f7f408fb9439a5cc5e393a6
