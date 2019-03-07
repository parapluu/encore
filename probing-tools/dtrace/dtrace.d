#pragma D option quiet

typedef struct pony_ctx_t
{
  void* scheduler;
  void* current;
};

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

int did_run_probe[string];

BEGIN {
}

pony$target::: /did_run_probe[probename] != 1/ {
  did_run_probe[probename] = 1;
}

encore$target::: /did_run_probe[probename] != 1/ {
  did_run_probe[probename] = 1;
}

pony$target:::actor-msg-send {
	@counter[probename] = count();
}

// arg[0] is scheduler, arg[1] is actor
pony$target:::actor-scheduled {
  cpus[arg1].cpu = cpu; // current CPU of the actor
  diagnostics.schedulings++;
	total_schedulings++;
	//@schedulers_for_actor[args[0], args[1]] = count();
}

pony$target:::work-steal-successful {
  diagnostics.cpu_jumps = (cpus[arg0].cpu != cpus[arg2].cpu) ? diagnostics.cpu_jumps+1 : diagnostics.cpu_jumps;
  diagnostics.successful_steals++;
  diagnostics.steal_attempts++;

  @steal_success_count[arg0] = count();
	@successful_steal_from_scheduler[arg0, arg1] = count();
	@stolen_actor[arg2] = count();
}

pony$target:::work-steal-failure {
  diagnostics.failed_steals++;
  diagnostics.steal_attempts++;
  @steal_fail_count[arg0] = count();
	@failed_steal_from_scheduler[arg0, arg1] = count();
}

encore$target:::closure-create {}

encore$target:::future-create {
  @counter[probename] = count();
  // Used for lifetime of a future
  self->future_create_starttime[arg1] = vtimestamp;
}

encore$target:::future-block {
  ctx = (struct pony_ctx_t*)copyin(arg0, sizeof(struct pony_ctx_t));
  actorPointer = (uintptr_t)ctx->current;
  @counter[probename] = count();
  @future_block[arg1] = count();
  @actor_blocked[actorPointer] = count();
  @future_blocked_actor[arg1, actorPointer] = count();
  // Used for duration of a block
  self->future_block_starttime[arg1, arg0] = vtimestamp;
}

encore$target:::future-unblock {
  ctx = (struct pony_ctx_t*)copyin(arg0, sizeof(struct pony_ctx_t));
  actorPointer = (uintptr_t)ctx->current;
  @counter[probename] = count();
  @future_block_lifetime[arg1, actorPointer] = sum(vtimestamp - self->future_block_starttime[arg1, arg0]);
}

encore$target:::future-chaining {
  @counter[probename] = count();
  @future_chaining[arg1] = count();
}

encore$target:::future-fulfil-start {
  @counter[probename] = count();
}

encore$target:::future-fulfil-end {
  @counter[probename] = count();
}

encore$target:::future-get {
  ctx = (struct pony_ctx_t*)copyin(arg0, sizeof(struct pony_ctx_t));
  actorPointer = (uintptr_t)ctx->current;
  @future_get[actorPointer, arg1] = count();
  @counter[probename] = count();
}

encore$target:::future-destroy {
  @counter[probename] = count();
  @future_lifetime[arg1] = sum(vtimestamp - self->future_create_starttime[arg1]);
}

encore$target:::method-entry {
  ctx = (struct pony_ctx_t*)copyin(arg0, sizeof(struct pony_ctx_t));
  actorPointer = (uintptr_t)ctx->current;
  // target pointer == the ctx current actor?
  if (arg1 == actorPointer) {
    self->function_time[arg1, arg2] = vtimestamp;
  }
}

encore$target:::method-exit {
  ctx = (struct pony_ctx_t*)copyin(arg0, sizeof(struct pony_ctx_t));
  actorPointer = (uintptr_t)ctx->current;
  // target pointer == the ctx current actor?
  if (arg1 == actorPointer) {
    name = copyinstr(arg2);
    @function_time[arg1, name] = sum(vtimestamp - self->function_time[arg1, arg2]);
  }
}

END {
	printf("==========================================\n");
	printf("\t\tFUTURES\n");
	printf("==========================================\n");
  printf("=== COUNTS ===\n");
  printa("%s\t%@1u\n", @counter);

	if (did_run_probe["future-create"]) {
	  printf("\n=== FUTURE_LIFETIME ===\n");
	  printf("Future id\t\tLifetime (nanoseconds)\n");
	  printa("%d\t\t%@1u\n", @future_lifetime);
	}
	if (did_run_probe["future-block"]) {
    printf("\n=== FUTURE_BLOCKED_ACTOR_LIFETIME ===\n");
	  printf("Future id\t\tActor id\t\tLifetime (nanoseconds)\n");
	  printa("%d\t\t%d\t\t%@1u\n", @future_block_lifetime);

  	printf("\n=== FUTURE_BLOCKED_ACTOR ===\n");
  	printf("Future id\t\tActor id\t\tCount\n");
  	printa("%d\t\t%d\t\t%@2u\n", @future_blocked_actor);

	  printf("\n=== NUMBER OF TIMES AN ACTOR IS BLOCKED ===\n");
	  printf("Actor id\t\tCount\n");
	  printa("%d\t\t%@2u\n", @actor_blocked);

		printf("\n=== NUMBER OF TIMES A FUTURE BLOCKS ===\n");
	  printf("Future id\t\tCount\n");
	  printa("%d\t\t%@2u\n", @future_block);
	}

  if (did_run_probe["future-get"]) {
    printf("\n=== NUMBER OF TIMES AN ACTOR DOES GET ===\n");
	  printf("Actor id\t\tFuture id\t\tCount\n");
	  printa("%d\t\t%d\t\t%@2u\n", @future_get);
  }

	if (did_run_probe["future-chaining"]) {
	  printf("\n=== NUMBER OF TIMES A FUTURE IS CHAINED ===\n");
	  printf("Future id\t\tCount\n");
	  printa("%d\t\t%@2u\n", @future_chaining);
	}

	if (did_run_probe["work-steal-successful"] || did_run_probe["work-steal-failure"]) {
		printf("==========================================\n");
		printf("\t\tSTEALS\n");
		printf("==========================================\n");
		printf("\nTOTAL\n");
		printf("Attempted\tSuccessful\tFailed\n");
		printf("%d\t\t%d\t\t%d\n",
										diagnostics.steal_attempts,
										diagnostics.successful_steals,
										diagnostics.failed_steals);

		printf("\nSUCCESSIVE STEALS\n");
		printf("Scheduler ID\tCount\n");
		printa("%d%@7u\n", @steal_success_count);

		printf("\nFAILED STEALS\n");
		printf("Scheduler ID\tCount\n");
		printa("%d%@7u\n", @steal_fail_count);

		printf("\nSTEALS BETWEEN SCHEDULERS\n");
		printf("Stolen by\tStolen from\tCount\n");
		printa("%d\t%d%@7u\n", @successful_steal_from_scheduler);

		printf("\nFAILS BETWEEN SCHEDULERS\n");
		printf("Attempted by\tTarget\t\tCount\n");
		printa("%d\t%d%@7u\n", @failed_steal_from_scheduler);

		printf("\nSTOLEN ACTORS\n");
		printf("Actor ID\tTimes stolen\n");
		printa("%d%@7u\n", @stolen_actor);

	  printf("\nCORE SWITCHES: %d\n", diagnostics.cpu_jumps);
	}
	if (did_run_probe["method-entry"]) {
    printf("==========================================\n");
		printf("\t\tMETHODS\n");
		printf("==========================================\n");

		printf("\nTIME SPENT IN METHODS (Nanoseconds)\n");
    printf("Actor id\t\tMethod name\t\tDuration\n");
		printa("%d\t\t%s\t\t\t%@u\n", @function_time);
	}
}
