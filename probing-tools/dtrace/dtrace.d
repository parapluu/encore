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
	@counts[probename] = count();
}

// arg[0] is scheduler, arg[1] is actor
pony$target:::actor-scheduled {
  cpus[arg1].cpu = cpu; // current CPU of the actor
}

pony$target:::work-steal-successful {
  if (cpu != cpus[arg2].cpu) {
    @counts["core-switches"] = count();
  }

  @counts[probename] = count();
  @counts["work-steal-attempt"] = count();
  @steal_success_count[arg0] = count();
	@successful_steal_from_scheduler[arg0, arg1] = count();
	@stolen_actor[arg2] = count();
}

pony$target:::work-steal-failure {
  @counts["work-steal-attempt"] = count();
  @counts[probename] = count();
  @steal_fail_count[arg0] = count();
	@failed_steal_from_scheduler[arg0, arg1] = count();
}

encore$target:::closure-create {}

encore$target:::future-create {
  @counts[probename] = count();
  // Used for lifetime of a future
  future_create_starttime[arg1] = timestamp;
}

encore$target:::future-block {
  ctx = (struct pony_ctx_t*)copyin(arg0, sizeof(struct pony_ctx_t));
  actorPointer = (uintptr_t)ctx->current;
  @counts[probename] = count();
  @future_block[arg1] = count();
  @actor_blocked[actorPointer] = count();
  @future_blocked_actor[arg1, actorPointer] = count();
  // Used for duration of a block
  future_block_starttime[arg1, actorPointer] = timestamp;
}

encore$target:::future-unblock {
  ctx = (struct pony_ctx_t*)copyin(arg0, sizeof(struct pony_ctx_t));
  actorPointer = (uintptr_t)ctx->current;
  @counts[probename] = count();
  @future_block_lifetime[arg1, actorPointer] = sum(timestamp - future_block_starttime[arg1, actorPointer]);
}

encore$target:::future-chaining {
  @counts[probename] = count();
  @future_chaining[arg1] = count();
}

encore$target:::future-fulfil-start {
  @counts[probename] = count();
}

encore$target:::future-fulfil-end {
  @counts[probename] = count();
}

encore$target:::future-get {
  ctx = (struct pony_ctx_t*)copyin(arg0, sizeof(struct pony_ctx_t));
  actorPointer = (uintptr_t)ctx->current;
  @future_get[actorPointer, arg1] = count();
  @counts[probename] = count();
}

encore$target:::future-destroy {
  @counts[probename] = count();
  @future_lifetime[arg1] = sum(timestamp - future_create_starttime[arg1]);
}

// encore$target:::method-entry {
//   ctx = (struct pony_ctx_t*)copyin(arg0, sizeof(struct pony_ctx_t));
//   actorPointer = (uintptr_t)ctx->current;
//   // target pointer == the ctx current actor?
//   if (arg1 == actorPointer) {
//     function_time[arg1, arg2] = timestamp;
//   }
// }
//
// encore$target:::method-exit {
//   ctx = (struct pony_ctx_t*)copyin(arg0, sizeof(struct pony_ctx_t));
//   actorPointer = (uintptr_t)ctx->current;
//   // target pointer == the ctx current actor?
//   if (arg1 == actorPointer) {
//     name = copyinstr(arg2);
//     @function_time[arg1, name] = sum(timestamp - function_time[arg1, arg2]);
//   }
// }

END {
  if ($$1 == "XML") {
    printf("<root>\n");
    printf("<counts>\n");
    printa("\t<%s count=\"%@u\" />\n", @counts);
    printf("</counts>\n");

    if (did_run_probe["future-create"]) {
      printf("<futures>\n");
      printa("\t<future>\n\t\t<id>%d</id>\n\t\t<duration>%@u</duration>\n\t</future>\n", @future_lifetime);
      printf("</futures>\n");
    }

    if (did_run_probe["future-block"]) {
      printf("<future-blocks>\n");
      printa("\t<future-block-lifetime>\n\t\t<future><id>%d</id></future>\n\t\t<actor><id>%d</id></actor>\n\t\t<duration>%@u</duration>\n\t</future-block-lifetime>\n", @future_block_lifetime);
      printa("\t<future-block-actor-count>\n\t\t<future><id>%d</id></future>\n\t\t<actor><id>%d</id></actor>\n\t\t<count>%@u</count>\n\t</future-block-actor-count>\n", @future_blocked_actor);
      printa("\t<future-block-count>\n\t\t<future><id>%d</id></future>\n\t\t<count>%@u</count>\n\t</future-block-count>\n", @future_block);
      printa("\t<actor-block-count>\n\t\t<actor><id>%d</id></actor>\n\t\t<count>%@u</count>\n\t</actor-block-count>\n", @actor_blocked);
      printf("</future-blocks>\n");
    }

    if (did_run_probe["future-get"]) {
      printf("<future-gets>\n");
      printa("\t<future-get>\n\t\t<actor>\n\t\t\t<id>%d</id>\n\t\t</actor>\n\t\t<future>\n\t\t\t<id>%d</id>\n\t\t</future>\n\t\t<count>%@u</count>\n\t</future-get>\n", @future_get);
      printf("</future-gets>\n");
    }

    if (did_run_probe["future-chaining"]) {
      printf("<future-chainings>\n");
      printa("\t<future-chaining>\n\t\t<future><id>%d</id></future>\n\t\t<count>%@u</count>\n\t</future-chaining>\n", @future_chaining);
      printf("</future-chainings>\n");
  	}

    if (did_run_probe["work-steal-successful"]) {
      printf("<work-steal-successes>\n");
      printa("\t<work-steal-success>\n\t\t<scheduler>\n\t\t\t<id>%d</id>\n\t\t</scheduler>\n\t\t<count>%@u</count>\n\t</work-steal-success>\n", @steal_success_count);
      printf("</work-steal-successes>\n");

      printf("<work-steal-success-from>\n");
      printa("\t<work-steal-success>\n\t\t<scheduler>\n\t\t\t<id>%d</id>\n\t\t</scheduler>\n\t\t<victim>%d</victim>\n\t\t<count>%@u</count>\n\t</work-steal-success>\n", @successful_steal_from_scheduler);
      printf("</work-steal-success-from>\n");
    }

    if (did_run_probe["work-steal-failure"]) {
      printf("<work-steal-failures>\n");
      printa("\t<work-steal-failure>\n\t\t<scheduler>\n\t\t\t<id>%d</id>\n\t\t</scheduler>\n\t\t<count>%@u</count>\n\t</work-steal-failure>\n", @steal_fail_count);
      printf("</work-steal-failures>\n");

      printf("<work-steal-failure-from>\n");
      printa("\t<work-steal-failure>\n\t\t<scheduler>\n\t\t\t<id>%d</id>\n\t\t</scheduler>\n\t\t<victim>%d</victim>\n\t\t<count>%@u</count>\n\t</work-steal-failure>\n", @failed_steal_from_scheduler);
      printf("</work-steal-failure-from>\n");
    }

    if (did_run_probe["work-steal-success"] || did_run_probe["work-steal-failure"]) {
      printf("<actor-stolen>\n");
      printa("\t<actor>\n\t\t<id>%d</id>\n\t\t<count>%@u</count>\n\t</actor>\n", @stolen_actor);
      printf("</actor-stolen>\n");
    }

    // if (did_run_probe["method-entry"]) {
    //   printf("<methods>\n");
    //   printa("\t<method>\n\t\t<actor>\n\t\t\t<id>%d</id>\n\t\t</actor>\n\t\t<name>%s</name>\n\t\t<duration>%@u</duration>\n\t</method>\n", @function_time);
    //   printf("</methods>");
  	// }

    printf("</root>");
  } else {
    printf("--- COUNTS ---\n");
    printa("%s\t%@1u\n", @counts);
    printf("Core switches:\t%d\n", diagnostics.cpu_jumps);
    printf("//---------- FUTURES ------------//\n");
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
      printf("\n--- Total number of times an actor calls get on a future ---\n");
  	  printf("Actor id\t\tFuture id\t\tCount\n");
  	  printa("%d\t\t%d\t\t%@u\n", @future_get);
    }

  	if (did_run_probe["future-chaining"]) {
  	  printf("\n--- Number of times a future is chained ---\n");
  	  printf("Future id\t\tCount\n");
  	  printa("%d\t\t%@2u\n", @future_chaining);
  	}

  	if (did_run_probe["work-steal-successful"] || did_run_probe["work-steal-failure"]) {
      printf("\n//---------- STEALS ------------//\n");

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
  	// if (did_run_probe["method-entry"]) {
    //   printf("\n//---------- METHODS ------------//\n");
    //
  	// 	printf("\n--- Time spent in methods\n");
    //   printf("Actor id\t\tMethod name\t\tDuration (Nanoseconds)\n");
  	// 	printa("%d\t\t%s\t\t\t%@u\n", @function_time);
  	// }
  }
}
