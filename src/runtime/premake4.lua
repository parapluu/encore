solution "encore_runtime"

  configuration "Debug"
    targetdir "bin/debug"
    objdir "obj/debug"

  configuration "Release"
    targetdir "bin/release"
    objdir "obj/release"
    defines "NDEBUG"
    buildoptions {
      "-O3",
      "-flto"
      }
    linkoptions {
      "-flto",
      "-fuse-ld=gold",
      }

  configuration "macosx"
    -- set these to suppress clang warning about -pthread being unused
    buildoptions "-Qunused-arguments"
    linkoptions "-Qunused-arguments"

project "closure"
  configurations {"Debug", "Release"}
  kind "StaticLib"
  language "C"
  includedirs "pony/inc/"
  files {
    "closure/closure.h",
    "closure/closure.c"
  }

project "set"
  configurations {"Debug", "Release"}
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
