solution "ponyrt"
  language "C"

  includedirs {
    "inc/"
    }

  configurations {"Debug", "Release"}

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

project "pony"
  kind "StaticLib"
  links { "future" }

  buildoptions {
    "-mcx16",
    "-pthread",
    "-march=native"
    }

  linkoptions {
    "-lm",
    "-pthread"
    }

  flags {
    "ExtraWarnings",
    "FatalWarnings",
    "Symbols"
    }

  include("src/")
  include("examples/")
  include("utils/")
  include("test/")

project "closure"
  kind "StaticLib"
  files {
    "../closure/closure.h",
    "../closure/closure.c"
  }

project "set"
  kind "StaticLib"
  files {
    "../set/set.c"
  }

project "future"
  kind "StaticLib"
  links { "closure", "set" }
  buildoptions { "-Wno-deprecated-declarations", "-std=gnu11" }
  includedirs { "../closure", "../set", "src/sched" }
  files {
    "../future/future.c",
    "../future/future_actor.c",
    "../future/tit_eager.c",
    "../future/tit_lazy.c"
  }

