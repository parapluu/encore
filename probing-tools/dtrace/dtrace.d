#pragma D option quiet

struct actor_info {
	uint64_t cpu;
  uint32_t steals;
  uint32_t jumps;
};

struct diagnostics {
  uint32_t successful_steals;
  uint32_t failed_steals;
  uint32_t steal_attempts;
  uint32_t cpu_jumps;
  uint32_t schedulings;
};

struct diagnostics diagnostics;
struct actor_info cpus[int64_t];	/* declare cpus as an associative array */

BEGIN
{

}

pony$target:::actor-alloc { }
pony$target:::actor-msg-send { }
pony$target:::actor-msg-run { }


// arg[0] is scheduler, arg[1] is actor
pony$target:::actor-scheduled
{

  cpus[arg1].cpu = cpu; // current CPU of the actor
  diagnostics.schedulings++;
	total_schedulings++;
	@schedulers_for_actor[args[0], args[1]] = count();
}

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


pony$target:::core-jump { }
	/**
   * Fired when a scheduler succesfully steals a job
   * @param scheduler is the scheduler that stole the job
   * @param victim is the victim that the scheduler stole from
   * @param actor is actor that was stolen from the victim
   */
pony$target:::work-steal-successful
{
  diagnostics.cpu_jumps = cpus[arg0].cpu != cpus[arg2].cpu ? diagnostics.cpu_jumps+1 : diagnostics.cpu_jumps;
  diagnostics.successful_steals++;
  diagnostics.steal_attempts++;

  @steal_success_count[arg0] = count();
	@successful_steal_from_scheduler[arg0, arg1] = count();
	@stolen_actor[arg2] = count();
}

/**
 * Fired when a scheduler fails to steal a job
 * @param scheduler is the scheduler that attempted theft
 * @param victim is the victim that the scheduler attempted to steal from
 */
pony$target:::work-steal-failure
{
  diagnostics.failed_steals++;
  diagnostics.steal_attempts++;
  @steal_fail_count[arg0] = count();
	@failed_steal_from_scheduler[arg0, arg1] = count();
}

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


END {
	printf("==========================================\n\t\tSTEALS\n==========================================\n");
	printf("\nTOTAL\n");
	printf("Attempted\tSuccessful\tFailed\n");
	printf("%d\t\t%d\t\t%d\n",
									diagnostics.steal_attempts,
									diagnostics.successful_steals,
									diagnostics.failed_steals);

	printf("\nSUCCESSIVE STEALS\n");
	printf("Scheduler ID\tCount\n");
	printa("%d%@8u\n", @steal_success_count);

	printf("\nFAILED STEALS\n");
	printf("Scheduler ID\tCount\n");
	printa("%d%@8u\n", @steal_fail_count);

	printf("\nSTEALS BETWEEN SCHEDULERS\n");
	printf("Stolen by\tStolen from\tCount\n");
	printa("%d\t%d%@8u\n", @successful_steal_from_scheduler);

	printf("\nFAILS BETWEEN SCHEDULERS\n");
	printf("Attempted by\tTarget\t\tCount\n");
	printa("%d\t%d%@8u\n", @failed_steal_from_scheduler);

	printf("\nSTOLEN ACTORS\n");
	printf("Actor ID\tTimes stolen\n");
	printa("%d%@8u\n", @stolen_actor);

  printf("\nCORE SWITCHES: %d\n", diagnostics.cpu_jumps);

	printf("\n==========================================\n\t\tSCHEDULING\n==========================================\n");
	printf("\nTOTAL SCHEDULINGS: %d\n", diagnostics.schedulings);

	printf("\nDISTRIBUTION\n");
	printf("Scheduler ID\tActor ID\tCount\n");
	printa("%d\t%d%@8u\n", @schedulers_for_actor);
}
