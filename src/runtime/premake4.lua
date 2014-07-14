solution "encore_runtime"

project "closure"
  kind "StaticLib"
  language "C"
  includedirs "pony/inc/"
  files {
    "closure/closure.h",
    "closure/closure.c"
  }

project "set"
  kind "StaticLib"
  language "C"
  includedirs "pony/inc/"
  files {
    "set/set.c"
  }

project "future"
  configurations {"Debug", "Release"}
  kind "StaticLib"
  language "C"
  links { "closure", "set" }
  buildoptions { "-Wno-deprecated-declarations", "-std=gnu11" }
  includedirs { "closure", "set", "pony/inc", "pony/src/sched" }
  files {
    "future/future.c",
    "future/future_actor.c",
    "future/tit_eager.c",
    "future/tit_lazy.c"
  }
