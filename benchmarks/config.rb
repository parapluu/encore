# Amount of times each benchmark configuration will be executed
REPS = 5

# The tool that will measure execution time and load of each program
# that is executed, result saved in data files. If you change this
# tool to another, please make sure that the parsing routines in
# run.rb works for the new tool.
TIMEPATH = "/usr/bin/time"
DEPENDENCIES = [TIMEPATH] # Add dependencies to this list
OS_SUPPORT = ["linux"]
TIME = "#{TIMEPATH} --verbose"

WD = Dir.pwd # Working directory
VERBOSE = false
SILENT = false

# datafile settings
DATE = true # puts current date and time in top of data files if true
THREADS = true # stores amount of ponythreads used in data file
INCLUDE_OUTPUT = true # includes program output in data file

PONYTHREADS_DEFAULT = 2 # default amount of ponythreads to use

UNUSED = [0]

PL_FANNKUCH_CONF = [
  ["n=7", [7]],
  ["n=8", [8]],
  ["n=9", [9]],
  ["n=10", [10]],
  ["n=11", [11]],
  ["n=12", [12]]
]

PL_CHAMENEOS_CONF = [
  ["2mil_pl_config", [2000000]],
  ["4mil_pl_config", [4000000]],
  ["2mil, 20chameneos", [2000000, 20]],
  ["4mil, 20chameneos", [4000000, 20]],
]

PL_BENCHMARKS = [
  ["pl_chameneos_scala", PL_CHAMENEOS_CONF],
  ["pl_chameneos_erlang", PL_CHAMENEOS_CONF],
  ["pl_chameneos_encore", PL_CHAMENEOS_CONF],
  ["pl_fannkuch_encore", PL_FANNKUCH_CONF],
  ["pl_fannkuch_encore_foreach", PL_FANNKUCH_CONF],
  ["pl_fannkuch_erlang", PL_FANNKUCH_CONF],
  ["pl_fannkuch_scala", PL_FANNKUCH_CONF]
]

GC_GCBENCH_CONF = [
  ["gc_bench", UNUSED]
]

GC_BENCHMARKS = [
  ["gc_gcbench", GC_GCBENCH_CONF],
]

IGNORES = [

]

BENCHMARKS = PL_BENCHMARKS + GC_BENCHMARKS
