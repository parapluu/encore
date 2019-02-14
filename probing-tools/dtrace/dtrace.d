#pragma D option quiet

BEGIN {
	printf("Dtrace started!");
}

END {
	printf("Dtrace finished!");
}

pony$target:::actor-alloc { }
pony$target:::actor-msg-send { }
pony$target:::actor-msg-run { }
pony$target:::actor-scheduled { }
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
pony$target:::work-steal-successful {}
pony$target:::work-steal-failure {}

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
