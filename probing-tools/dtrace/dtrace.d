#pragma D option quiet

typedef struct pony_ctx_t
{
  void* scheduler;
  void* current;
};

struct actor_info {
	uint64_t cpu;
  uint32_t jumps;
};

struct diagnostics {
  uint32_t steal_attempts;
  uint32_t cpu_jumps;
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
	@count_future[probename] = count();
}

// arg[0] is scheduler, arg[1] is actor
pony$target:::actor-scheduled {
  cpus[arg1].cpu = cpu; // current CPU of the actor
}

pony$target:::work-steal-successful {
  diagnostics.cpu_jumps = (cpu != cpus[arg2].cpu) ? diagnostics.cpu_jumps+1 : diagnostics.cpu_jumps;
  diagnostics.steal_attempts++;
  @count_steals[probename] = count();
  @steal_success_count[arg0] = count();
	@successful_steal_from_scheduler[arg0, arg1] = count();
	@stolen_actor[arg2] = count();
}

pony$target:::work-steal-failure {
  diagnostics.steal_attempts++;
  @count_steals[probename] = count();
  @steal_fail_count[arg0] = count();
	@failed_steal_from_scheduler[arg0, arg1] = count();
}

encore$target:::closure-create {}

encore$target:::future-create {
  @count_future[probename] = count();
  // Used for lifetime of a future
  self->future_create_starttime[arg1] = vtimestamp;
}

encore$target:::future-block {
  ctx = (struct pony_ctx_t*)copyin(arg0, sizeof(struct pony_ctx_t));
  actorPointer = (uintptr_t)ctx->current;
  @count_future[probename] = count();
  @future_block[arg1] = count();
  @actor_blocked[actorPointer] = count();
  @future_blocked_actor[arg1, actorPointer] = count();
  // Used for duration of a block
  self->future_block_starttime[arg1, arg0] = vtimestamp;
}

encore$target:::future-unblock {
  ctx = (struct pony_ctx_t*)copyin(arg0, sizeof(struct pony_ctx_t));
  actorPointer = (uintptr_t)ctx->current;
  @count_future[probename] = count();
  @future_block_lifetime[arg1, actorPointer] = sum(vtimestamp - self->future_block_starttime[arg1, arg0]);
}

encore$target:::future-chaining {
  @count_future[probename] = count();
  @future_chaining[arg1] = count();
}

encore$target:::future-fulfil-start {
  @count_future[probename] = count();
}

encore$target:::future-fulfil-end {
  @count_future[probename] = count();
}

encore$target:::future-get {
  ctx = (struct pony_ctx_t*)copyin(arg0, sizeof(struct pony_ctx_t));
  actorPointer = (uintptr_t)ctx->current;
  @future_get[actorPointer, arg1] = count();
  @count_future[probename] = count();
}

encore$target:::future-destroy {
  @count_future[probename] = count();
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
	printf("//---------- FUTURES ------------//\n");
  printf("--- COUNTS ---\n");
  printa("%s\t%@1u\n", @count_future);

	if (did_run_probe["future-create"]) {
	  printf("\n--- Duration a future is alive ---\n");
	  printf("Future id\t\tLifetime (nanoseconds)\n");
	  printa("%d\t\t%@1u\n", @future_lifetime);
	}
	if (did_run_probe["future-block"]) {
    printf("\n--- Duration a future blocks an actor ---\n");
	  printf("Future id\t\tActor id\t\tLifetime (nanoseconds)\n");
	  printa("%d\t\t%d\t\t%@1u\n", @future_block_lifetime);

  	printf("\n--- Number of times an actor is blocked by a future ---\n");
  	printf("Future id\t\tActor id\t\tCount\n");
  	printa("%d\t\t%d\t\t%@2u\n", @future_blocked_actor);

	  printf("\n--- Total number of times an actor is blocked ---\n");
	  printf("Actor id\t\tCount\n");
	  printa("%d\t\t%@2u\n", @actor_blocked);

		printf("\n--- Total number of times a future blocks ---\n");
	  printf("Future id\t\tCount\n");
	  printa("%d\t\t%@2u\n", @future_block);
	}

  if (did_run_probe["future-get"]) {
    printf("\n--- Number of times an actor calls get ---\n");
	  printf("Actor id\t\tFuture id\t\tCount\n");
	  printa("%d\t\t%d\t\t%@2u\n", @future_get);
  }

	if (did_run_probe["future-chaining"]) {
	  printf("\n--- Number of times a future is chained ---\n");
	  printf("Future id\t\tCount\n");
	  printa("%d\t\t%@2u\n", @future_chaining);
	}

	if (did_run_probe["work-steal-successful"] || did_run_probe["work-steal-failure"]) {
    printf("\n//---------- STEALS ------------//\n");
		printf("\n--- COUNTS ---\n");
    printf("Attempted\t%d\n", diagnostics.steal_attempts);
    printf("Core switches:\t%d\n", diagnostics.cpu_jumps);
    printa("%s\t%@2u\n", @count_steals);

		printf("\n--- Number of times a scheduler successfully steals ---\n");
		printf("Scheduler id\t\tCount\n");
		printa("%d\t\t%@2u\n", @steal_success_count);

		printf("\n--- Number of times a scheduler fails to steal ---\n");
		printf("Scheduler id\t\tCount\n");
		printa("%d\t\t%@2u\n", @steal_fail_count);

		printf("\n--- Number of times a scheduler steals from another ---\n");
		printf("Stolen by\t\tStolen from\t\tCount\n");
		printa("%d\t\t%d\t\t%@2u\n", @successful_steal_from_scheduler);

		printf("\n--- Number of times a scheduelr fails to steal from another ---\n");
		printf("Attempted by\t\tTarget\t\tCount\n");
		printa("%d\t\t%d\t\t%@2u\n", @failed_steal_from_scheduler);

		printf("\n--- Number of times an actor is stolen ---\n");
		printf("Actor id\t\tTimes stolen\n");
		printa("%d\t\t%@2u\n", @stolen_actor);
	}
	if (did_run_probe["method-entry"]) {
    printf("\n//---------- METHODS ------------//\n");

		printf("\n--- Time spent in methods\n");
    printf("Actor id\t\tMethod name\t\tDuration (Nanoseconds)\n");
		printa("%d\t\t%s\t\t\t%@u\n", @function_time);
	}
}
