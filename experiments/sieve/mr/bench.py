#!/usr/bin/env python

# ----------------------------------------------------------------
# Run prime sieve with various input sizes and thread numbers
#
# NOTE!!!!!!!!!!!!!!!!
# 
# If you run this program on OSX, first do "brew install gtime"
# ----------------------------------------------------------------

from subprocess import call, check_output

PROG = check_output(["which", "gtime"])[:-1]
if not PROG[0] == "/":
   PROG = check_output(["which", "time"])[:-1]
   if not PROG[0] == "/":
      PROG = None

DEPTHS       = {1:1, 2:3, 3:7, 4:15, 5:31, 6:63}
PROGRAM_NAME = "mr"
PROGRAM_DIR  = "./"
OUTPUT_DIR   = "./bench"
INPUT_SIZES  = [1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608, 16777216, 33554432, 67108864, 134217728, 268435456, 536870912]

if PROG:
   """Perform benchmarking tests and save output"""
   call(["mkdir", "-p", OUTPUT_DIR])

   for size in INPUT_SIZES:
      for depth in DEPTHS:
         threads = DEPTHS[depth]
         call([PROG, 
               "--verbose",
               "--output=" + OUTPUT_DIR + "/" + PROGRAM_NAME + "-" + str(size) + "-" + str(threads) + ".txt",
               PROGRAM_DIR + PROGRAM_NAME,
               str(size),
               str(depth),
               "--ponythreads " + str(threads)]) 
else:
   print "Could not find usable versions of time or gtime! Plese read the sources of this file."
