A Probing Toolset for the Encore Programming Language

Using DTrace and SystemTap

Note that is this is not the full repport but ony contains certain exscripts from it to give sufficient instructions 
on how the tools work and what can be expected from them. If you only want the lines to code to compile the probes
or run the parser, please see the section Prerequisites.

### Ardalan Samimi, Ulf Sigvardsson, Joy van den Eijkhof

### January 2019

```
Abstract
Encore is an actor based programming language in development at Uppsala University. This
README  describes the of two probing tools, intended to quantify the internal char-
acteristics of a running Encore program, using the tracing systems SystemTap and DTrace.
They are generators of raw data that can be processed to gain insight into the behaviour of
the Encore compiler.
```

## Contents


- 1 Introduction
   - 1.1 SystemTap and DTrace
- 2 Tool specification
   - 2.1 Prerequisites
      - 2.1.1 Setup
      - 2.1.2 Example use
- 3 Results
   - 3.1 DTrace
      - 3.1.1 Errors
   - 3.2 SystemTap
      - 3.2.1 Errors
   - 3.3 Data Output
- 5 Bibliography
- Appendices
- A Contributions
   - A.1 Joy van den Eijkhof
   - A.2 Ulf Sigvardsson
   - A.3 Ardalan Samimi
- B Source Code, DTrace Probes
- C Source Code, SystemTap Probes
- D XML Tags Specification
- E Example Output

### 1.1 SystemTap and DTrace

DTrace is a dynamic tracing framework for MacOS, while SystemTap is a tracing framework for
Linux based operating systems. These tracing frameworks allow for probing and tracing the be-
haviour of a computer program. Aside from allowing for actions such as tracing method calls in
user space, measuring the number of objects created of a specific class et cetera, the primary benefit
of a probing tool is the ability to measure kernel level execution within a user space program, not
usually accessible by the programmer. System calls such as memory allocation, scheduler activity
and CPU diagnostics can be probed, giving a deeper insight of the behaviour of a program or system.

Tracing is done by writing scripts and applying them to a program to trace. DTrace scripts are
written in the D language [4] and SystemTap scripts in the SystemTap Scripting Language[2]. These
scripts are written using probes. A probe is a handle into a a specific application or kernel event.
The probe fires when the code related to the event is executed.

A script consists of dynamic probes, each associated with an action. When the condition of a probe
is met, the probe fires.

The general syntax of a DTrace probe is


provider:module:function:name
/predicate/
{
//action statements
}

The work to be done when a probe is fired is written in the probe body. Thefunctionfield
specifies the function to be probed, such asmalloc, andnamespecifies what action to trace, e.g.
entry, firing at the entry of the function. Probes may take arguments that can then be referenced
in the probe body. Additionally, a slew of built-in variables such as the process ID of the current
process, the current CPU and the current thread executing is accessible in both tracing languages.
By using these variables and the arguments, data regarding the traced program can be accumulated
and presented in a comprehensive way. Furthermore, a predicate can be specified in order to discard
probes not of interest.

The SystemTap equivalence is

probe event
{
//action statements
}

The event field contains information regarding what tracing event is being probed, and multiple
events can be attached to the same probe and separated by a comma. It can, for example, specify
that it is probing for a kernel function in a specific kernel source file, or for the event that a static
probe is fired. The probe body contains instructions to be executed, and works mostly the same
way as for DTrace.

## 2 Tool specification

This section describes the tools that has been developed. The DTrace and SystemTap scripts both have
two output modes, either outputting data in a human readable format or in XML format.

### 2.1 Prerequisites

The DTrace tool requires a computer running the operating system MacOS with DTrace installed.
The SystemTap tool requires a Linux operating system with SystemTap installed. To run the
parser, NodeJS and the Node Package Manager is required.

#### 2.1.1 Setup

In order for the probing tools to function, the Encore compiler must be built with the probes en-
abled. This can be done by the following command:make use=dtrace. Furthermore, for certain
probes to function on MacOS, System Integrity Protection must be turned off[6].

As the parser uses various external packages, run the following command in theprobing-tools/parser
subdirectory to install these.

```
npm install
```
#### 2.1.2 Example use

To probe a program using DTrace and retrieve the information in XML format, from the command
line, run

```
dtrace -o ../output.xml -Cs dtrace.d -c ./[target binary] XML
```
This assumes that the working directory isprobing-tools/dtrace, paths would have to be
modified accordingly if this is not the case. To display the default output, substitute theXML
argument withNORMAL.

The flags used in this example are:

- -o: redirects the output of the script tooutput.xml.
- -C: enables preprocessing of C code, enabling C structs et cetera.
- -s: specifies a DTrace script to use when probing.


- -c: specifies an executable for DTrace to probe.

To probe the program using SystemTap, from the command line, run

```
stap systemTapXML.stp -c [target binary} -o output.XML
```
This assumes that the working directory isprobing-tools/systemtap. To display the default
output, run with the SystemTap scriptsystemTap.stpinstead.

The flags used in the example are:

- -c: specifies the compiled Encore program, whose execution determines when the probing
    begins and ends.
- -o: specifies the output file that will contain the output from the probing tool, and not from
    the program being probed.

To parse the XML output, simply run the following command:

```
node parser/index.js /path/to/output.xml
```
This assumes that the working directory isprobing-tools.


## 3 Results

Here follows the results of probing the selected Savina benchmark tests with both probing tools.
Errors encountered are covered as well as suggestions on how to avoid them.

### 3.1 DTrace

When applying the DTrace tool to the existing Savina Benchmark Suite Encore programs, these
were the results.

#### 3.1.1 Errors

Two common errors are related toaggregation buffer sizeanddynamic variable space size. These
correspond to the number of entries in a given aggregation and the maximum size of an associated
array, respectively. Since the nature of Encore, as an actor based language, often require vast num-
bers of actors, the default limits of these variables are often surpassed.

To modify the maximum aggregation size for the DTrace script, option-x aggsize=Nmmust be
passed to DTrace, whereNis the desired aggregation size in megabytes. For the benchmarks tested,
16MB has been sufficient to avoid any errors.

The maximum dynamic variable size is modified similarly with-x dynvarsize=Nm. A value of
8MB allowed for the benchmarks to pass.


### 3.2 SystemTap

When applying the SystemTap tool to the existing Savina Benchmark Suite Encore programs, these
were the results.

#### 3.2.1 Errors

As these benchmark tests are meant to be stress tests, some programs create a multitude of futures or
actors, or create some other kind of edge case to explore how Encore fares under those circumstances.
Applying the SystemTap probing tool to some of these programs did not work. SystemTap has
certain limitations, such as how many of one probe it can handle, how much memory it allows
for its arrays and how many actions it is allowed to perform per dynamic probing event. When
these limitations are exceeded, the tracing script aborts its execution. Below are the the encountered
errors, and the reason for the failure and what remedies were possible to make SystemTap functional.

**Probe overhead exceeded threshold** This is a run-time error, generated when the han-
dle time of some probe is greater than the allowed fraction of the total real time of the trac-
ing program. It was encountered when applying the probing script to the benchmark program
ThreadRing, which creates and sends a large amount of messages, resulting in the dynamic probe
actor-msg-sendtaking up too much of the total probing time. This can be remedied by ignoring
such time limits by executing the probing tool with the flags-gand --suppress-time-limits
[1]. However, the-gflag enables what is calledguru-mode, removing code and data memory
reference protection, and is not recommended for inexperienced users due to potentially unsafe
operations [2].

**MAXMAPENTRIES** Run-time error generated if any array exceeds the pre-defined, or default maxi-
mum number of rows. In the probing tool, no arrays have a predefined size, thus certain benchmark
tests create more than 2048 entries that is the default maximum. This error was encountered when
attempting to probe benchmark programs that generated either a lot of futures or actors, such
asForkJoin, which measures the time it takes to create and terminate actors, and does so by
creating a multitude of actors. It is possible to increase this maximum by recompiling the probing
tool with the flag-DMAXMAPENTRIE=NN, whereNNis the new threshold [11].

**MAXACTION** This is another run-time error that is generated due to too many actions occurring
within a probe, as a probe may at most execute 1000 instructions at any event. This was generated
inside of the final probeprobe end, executed upon the completion of the probing. Within it, all
data is printed usingforeach-loops. When applying the probing tool to benchmarks generating
a lot of actors or futures, each actor might be listed to count how many times it was stolen by a
scheduler for example, easily surpassing the default maximum. This threshold can be increased by
recompiling the probing tool with the flag-DMAXACTION=NN, whereNNis the new threshold [1].

**Transport failure** This error is generated when there are too many output printing state-
ments. As the probing output is entirely made up of printing statements, this can be expected to
occur for the same reason as withMAXACTION, namely too many actors or futures for which to
print collected data. This be avoided by running the probing tool with a greater trace buffer by
compiling with the flag-s NN, whereNNis the new threshold in MB. In this case, however,NN


must be between 1 and 4095[1], and as a result of this constraint certain benchmark tests could not
be probed.

### 3.3 Data Output

Assuming that the probing occurred without problems, the results from the probing will either
print out to standard output, or to an indicated output file. The XML output can then be parsed
using the provided parser, or handled as desired as any XML output can be. An example of a part
of an output handled by the created parser can be seen in Appendix E. It shows which schedulers
have been successful and unsuccessful in stealing work from another scheduler.

The actors, futures, and schedulers can be identified by their IDs (e.g. memory addresses), and are
presented as integers in the output, corresponding to these addresses. The goal was to be able to
map an actor’s address to the name of the active class that it was an instance of. However, due to
the way that these names is stored in Encore, our attempts were unsuccessful.

## 5 Bibliography

References

```
[1] William Cohen Don Domingo.Exhausted resource errors.
url:https://sourceware.org/systemtap/wiki/TipExhaustedResourceErrors
(visited on [visited on 03/25/2019]).
[2] William Cohen Don Domingo.SystemTap Beginners Guide.
url:https://sourceware.org/systemtap/SystemTap_Beginners_Guide/(vis-
ited on [visited on 03/22/2019]).
[3] William Cohen Don Domingo.SystemTap Tapset Reference Manual.
url:https://sourceware.org/systemtap/tapsets/index.html/(visited on
[visited on 03/22/2019]).
[4] The D Language Foundation.D Programming Language.
url:https://dlang.org(visited on [visited on 02/01/2019]).
[5] Shams Imam and Vivek Sarkar.Savina - An Actor Benchmark Suite. Rice University, Hous-
ton, USA., 2014.
[6] Joseph Keller.How to turn off System Integrity Protection on your Mac.
url:https://www.imore.com/how-turn-system-integrity-protection-
macos(visited on [visited on 03/26/2019]).
[7] Oracle.About DTrace.
url:http://dtrace.org/blogs/about/(visited on [visited on 02/01/2019]).
[8] Oracle.DTrace Aggregations.
url:https://docs.oracle.com/cd/E18752_01/html/819-5488/gcggh.html
(visited on [visited on 02/01/2019]).
[9] Oracle.DTrace Built-in Variables.
url:https : / / www. oracle. com / technetwork / server - storage / solaris /
dtrace-built-in-vars-137322.html(visited on [visited on 02/01/2019]).
```
[10] Oracle.DTrace Providers.
url:https : / / www. oracle. com / technetwork / server - storage / solaris /
dtrace-providers-140248.html(visited on [visited on 02/01/2019]).

[11] SystemTap Language Reference.
url:https://sourceware.org/systemtap/langref.pdf(visited on [visited on
03/25/2019]).

[12] The Encore Programming Language.
url:https://stw.gitbooks.io/the-encore-programming-language/content/
nutshell.html(visited on [visited on 02/01/2019]).


## Appendices

## A Contributions

This section covers each team member’s contribution to the project.

### A.1 Joy van den Eijkhof

Joy van den Eijkhof has written the SystemTap scripts, in close collaboration with Ardalan Samimi
and Ulf Sigvardsson to solve mutual problems in the different versions the probing tools, and to make
sure the same data is produced in a similar fassion so the tools could be consider interchangable.

### A.2 Ulf Sigvardsson

Ulf Sigvardsson has, together with Ardalan Samimi, written the DTrace script and in close col-
laboration with Joy van den Eijkhof to solve mutual problems, and make descisions regarding the
DTrace and SystemTap probing tools

### A.3 Ardalan Samimi

Ardalan Samimi has written the parser, and has together with Ulf Sigvardsson, written the DTrace
script in close collaboration with Joy van den Eijkhof to solve mutual problems, and make decisions
regarding the DTrace and SystemTap probing tools.


## B Source Code, DTrace Probes

pony$target:::actor-msg-send {
@counts[probename] = count();
}

```
Figure 2: DTrace implementation ofactor-msg-send
```
// arg0: scheduler
// arg1: the actor
pony$target:::actor-scheduled {
cpus[arg1].cpu = cpu;// current CPU of the actor
}

```
Figure 3: DTrace implementation ofactor-scheduled
```
// arg1: pointer to the future structure
encore
$target:::future-create {
@counts[probename] = count();
// Used for lifetime of a future
future-create-starttime[arg1] = timestamp;
}

```
Figure 4: DTrace implementation offuture-create
```
// arg0: actor context
// arg1: pointer to the future struct
encore$target:::future-block {
ctx = ( **struct** pony_ctx_t*)copyin(arg0, sizeof( **struct** pony_ctx_t));
actorPointer = (uintptr_t)ctx->current;
@counts[probename] = count();
@future_block[arg1] = count();
@actor_blocked[actorPointer] = count();
@future_blocked_actor[arg1, actorPointer] = count();
future_block_starttime[arg1, actorPointer] = timestamp;
}

```
Figure 5: DTrace implementation offuture-block.
```

// arg0: actor context
// arg1: pointer to future struct
encore$target:::future-unblock {
ctx = ( **struct** pony_ctx_t*)copyin(arg0, sizeof( **struct** pony_ctx_t));
actorPointer = (uintptr_t)ctx->current;
@counts[probename] = count();
@future_block_lifetime[arg1, actorPointer] =
sum(timestamp - future_block_starttime[arg1, actorPointer]);
}

```
Figure 6: DTrace implementation offuture-unblock
```
// arg0: actor context
// arg1: pointer to the future struct
// arg2: the type being returned by the future
encore$target:::future-chaining {
@counts[probename] = count();
@future_chaining[arg1] = count();
}

```
Figure 7: DTrace implementation offuture-chaining
```
// arg0: actor context
// arg1: pointer to the future struct
encore$target:::future-get {
ctx = ( **struct** pony_ctx_t*)copyin(arg0, sizeof( **struct** pony_ctx_t));
actorPointer = (uintptr_t)ctx->current;
@future_get[actorPointer, arg1] = count();
@counts[probename] = count();
}

```
Figure 8: DTrace implementation offuture-get
```
// arg1: pointer to the future struct
encore$target:::future-destroy {
@counts[probename] = count();
@future_lifetime[arg1] = sum(timestamp - future_create_starttime[arg1]);
}

```
Figure 9: DTrace implementation offuture-destroy
```

// arg0: the scheduler that stole the job
// arg1: the victim that the scheduler stole from
// arg2: actor that was stolen from the victim
pony$target:::work-steal-successful {
**if** (cpu != cpus[arg2].cpu) {
@counts["core-switches"] = count();
}

@counts[probename] = count();
@counts["work-steal-attempt"] = count();
@steal_success_count[arg0] = count();
@successful_steal_from_scheduler[arg0, arg1] = count();
@stolen_actor[arg2] = count();
}

```
Figure 10: DTrace implementation ofwork-steal-successful
```
// arg0: the scheduler that attempted theft $
// arg1: the scheduler failed to steal from
pony$target:::work-steal-failure {
@counts["work-steal-attempt"] = count();
@counts[probename] = count();
@steal_fail_count[arg0] = count();
@failed_steal_from_scheduler[arg0, arg1] = count();
}

```
Figure 11: DTrace implementation ofwork-steal-failure
```

## C Source Code, SystemTap Probes

probe process.mark("actor-msg-send") {
actor_msg_send <<< 1;
}

```
Figure 12: SystemTap implementation ofactor-msg-send
```
probe process.mark("actor-scheduled") {
actor = sprint($arg2)
cpus[actor] = cpu()
}

```
Figure 13: SystemTap implementation ofactor-scheduled
```
# $arg2: pointer to the future structure
probe process.mark("future-create") {
future_create <<< 1;
future = sprint($arg2)
future-create-starttime[future] = gettimeofday_ns()
}

```
Figure 14: SystemTap implementation offuture-create
```
# $arg1: pointer context
# $arg2: pointer to the future struct
probe process.mark("future-block") {
actor = sprint(@cast($arg1, "pony_ctx_t")->current)
future = sprint($arg2)
future_block <<< 1
future_block_count[future] <<< 1
actor_block_count[actor] <<< 1
future_block_actor[future, actor] <<< 1
list_future_block_lifetime[future, actor] = gettimeofday_ns()
}

```
Figure 15: SystemTap implementation offuture-block.
```

# $arg1: actor context
# $arg2: pointer to future struct
probe process.mark("future-unblock") {
actor = sprint(@cast($arg1, "pony_ctx_t")->current)
future = sprint($arg2)
future_unblock <<< 1
list_future_block_lifetime[future, actor] =
gettimeofday_ns()-list_future_block_lifetime[future, actor]
}

```
Figure 16: SystemTap implementation offuture-unblock
```
# $arg1: actor context
# $arg2: pointer to the future struct
# $arg3: the type being returned by the future
probe process.mark("future-chaining") {
future_chaining <<< 1
future = sprint($arg2)
chained_actor_future_list[future] <<< 1;
}

```
Figure 17: SystemTap implementation offuture-chaining
```
# $arg1: actor context
# $arg2: pointer to the future struct
probe process.mark("future-get") {
actor = sprint(@cast($arg1, "pony_ctx_t")->current)
future = sprint($arg2)
actor_get_future[actor, future] <<< 1
future_get <<< 1
}

```
Figure 18: SystemTap implementation offuture-get
```
probe process.mark("future-destroy") {
future_destroy <<< 1
future = sprint($arg2)
list_future_lifetime[future] = gettimeofday_ns()-list_future_lifetime[future]
}

```
Figure 19: SystemTap implementation offuture-destroy
```

probe process.mark("work-steal-successful") {
scheduler = sprint($arg1)
victim = sprint($arg2)
actor = sprint($arg3)

**if** (cpu() != cpus[actor]) {
core_switches <<< 1
cpus[actor] = cpu()
}
successful_steals <<< 1
total_steals <<< 1
steal_success_count[scheduler] <<< 1
stolen_actor[actor] <<< 1
successful_steal_from_scheduler[scheduler, victim] <<< 1
}

```
Figure 20: SystemTap implementation ofwork-steal-successful
```
# $arg1: the scheduler that attempted theft $
# $arg2: the scheduler failed to steal from
probe process.mark("work-steal-failure") {
scheduler = sprint($arg1)
victim = sprint($arg2)
failed_steals <<< 1;
total_steals <<< 1;
failed_steals_count[scheduler] <<< 1
scheduler_from_scheduler_fail[scheduler, victim] <<< 1
}

```
Figure 21: SystemTap implementation ofwork-steal-failure
```

## D XML Tags Specification

The XML tags used for the input file for the parser is described below. The document must start
and end with theroottag.

### id

Represents an id. Its value must be an integer.

```
< id >...</ id >
```
### duration

Represents a duration. Its value must be an integer.

```
< duration >...</ duration >
```
### future

Represents a future. Consists of at least the<id>element. It may also optionally contain
<duration>element.

< **future** >
< **id** >...</ **id** >
< **duration** >...</ **duration** >
</ **future** >

### actor

Represents an actor and consists of the<id>element.

```
< actor >
< id >...</ id >
</ actor >
```
### scheduler

Represents a scheduler and consists of the<id>element.

### futures

Encloses<future>elements and holds information on all futures created during the runtime. This
tag must not consist of any other elements.


```
< scheduler >
< id >...</ id >
</ scheduler >
```
```
< futures >
< future >
< id >X1</ id >
< duration >Y1</ duration >
</ future >
< future >
< id >X2</ id >
< duration >Y2</ duration >
</ future >
</ futures >
```
### counts

Encloses tags describing the number of times a probe has been fired. Child tags are of the format
seen below, whereXandY are integers andname-of-enumerationis the name of the probe
fired.

```
< counts >
< name-of-enumeration count="X" />
< name-of-enumeration count="Y" />
</ counts >
```
### future-blocks

This tag encapsulates data gathered on future blocking and consists of several child elements:
future-block-lifetime,future-block-actor-count,future-block-countand
actor-block-count.

### future-block-lifetime

Child element of the<future-blocks>tag. Describes how long a future has blocked a certain
actor. Consists of a<future>element, an<actor>element and the<duration>element.


```
< future-block-lifetime >
< future >
< id >...</ id >
</ future >
< actor >
< id >...</ id >
</ future >
< duration >...</ duration >
</ future-block-lifetime >
```
### future-block-actor-count

Child element of the<future-blocks>tag. Describes how many times a future has blocked a
certain actor. Consists of a<future>element, an<actor>element and the<count>element.

```
< future-block-actor-count >
< future >
< id >...</ id >
</ future >
< actor >
< id >...</ id >
</ future >
< count >...</ count >
</ future-block-actor-count >
```
### future-block-count

Child element of the<future-blocks>tag. Describes how many times a future has blocked in
total. Consists of a<future>element and the<count>element.

```
< future-block-count >
< future >
< id >...</ id >
</ future >
< count >...</ count >
</ future-block-count >
```
### actor-block-count

Child element of the<future-blocks>tag. Describes how many times an actor has been blocked
in total. Consists of an<actor>element and the<count>element.


```
< actor-block-count >
< actor >
< id >...</ id >
</ actor >
< count >...</ count >
</ actor-block-count >
```
### actor-block-count

Child element of the<future-blocks>tag. Describes how many times an actor has been blocked
in total. Consists of an<actor>element and the<count>element.

```
< actor-block-count >
< actor >
< id >...</ id >
</ actor >
< count >...</ count >
</ actor-block-count >
```
### future-gets

This tag encloses<future-get>elements.

### future-get

Child element of the<future-gets>element. Describes how many times an actor has called
geton a future. Consists of the<actor>,<future>and<count>elements.

```
< future-get >
< actor >< id >...</ id ></ actor >
< future >< id >...</ id ></ future >
< count >...</ count >
</ actor-block-count >
```
### future-chainings

This tag encloses<future-chaining>elements.

### future-chaining

Child element of the<future-chainings>element. Describes how many times a future has
been chained. Consists of the<future>and<count>elements.


```
< future-get >
< actor >< id >...</ id ></ actor >
< future >< id >...</ id ></ future >
< count >...</ count >
</ actor-block-count >
```
### work-steal-successes

This tag encloses<work-steal-success>elements.

### work-steal-success-from

This tag encloses<work-steal-success>elements, with the requirement that these elements
contains the<victim>element.

### work-steal-failures

This tag encloses<work-steal-failure>elements.

### work-steal-success-failure-from

This tag encloses<work-steal-failure>elements, with the requirement that these elements
contains the<victim>element.

### work-steal-success

Describes how many times a scheduler has successfully stolen work from another. Consists of the
<scheduler>and<count>elements. It may also contain the optional element<victim>.

```
< work-steal-success >
< scheduler >< id >...</ id ></ scheduler >
< victim >...</ victim >
< count >...</ count >
</ work-steal-success >
```
### work-steal-failure

Describes how many times a scheduler has failed to steal work from another. Consists of the
<scheduler>and<count>elements. It may also contain the optional element<victim>.


```
< work-steal-failure >
< scheduler >< id >...</ id ></ scheduler >
< victim >...</ victim >
< count >...</ count >
</ work-steal-failure >
```
### actor-stolen

Describes how many times an actor has switched scheduler. Consists of the<actor>and<count>
elements.

```
< actor-stolen >
< actor >< id >...</ id ></ actor >
< count >...</ count >
</ actor-stolen >
```

## E Example Output

------------------------------- Schedulers ------------------------------
-------------------------------------------------------------------------
| (index) | id | successfulSteals | failedSteals |
-------------------------------------------------------------------------
| 139717569259520 | '139717569259520' | 93 | 1 |
| 139717569259904 | '139717569259904' | 2 | 1 |
-------------------------------------------------------------------------

--------------------------- Work steal success ---------------------------
-----------------------------------------------------------
| (index) | scheduler | victim | count |
|---------------------------------------------------------|
| 0 | '139717569259520' | '139717569259904' | 93 |
| 1 | '139717569259904' | '139717569259520' | 2 |
-----------------------------------------------------------

--------------------------- Work steal failure ---------------------------
-----------------------------------------------------------
| (index) | scheduler | victim | count |
|---------------------------------------------------------|
| 0 | '139717569259520' | '139717569259904' | 1 |
| 1 | '139717569259904' | '0' | 1 |
-----------------------------------------------------------



















































