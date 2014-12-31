solution "ponyrt"
  includedirs {
    "inc/"
    }

  buildoptions {
    "-mcx16",
    "-pthread",
    "-march=native"
    }

  linkoptions {
    "-lm",
    "-pthread"
    }

  configurations {"Debug", "Release"}

  configuration "Debug"
    targetdir "bin/debug"
    objdir "obj/debug"
    buildoptions {
      "-ggdb",
      }

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
    -- This stupidity because of http://industriousone.com/topic/how-remove-flags-ldflags
    -- It manually removes the -Wl,-x flags that are erroneously inserted on OS X
    prebuildcommands "@if [ -d \"test/\" ]; then sed -e 's/-Wl,-x//g' -i '' test/*.make; fi"

project "pony"
  kind "StaticLib"
  links { "future" }

  flags {
    "ExtraWarnings",
--    "FatalWarnings",		
    "Symbols"
    }

  include("src/")
  include("examples/")
  include("utils/")
  include("test/")

project "closure"
  kind "StaticLib"
  language "C"
  files {
    "../closure/closure.h",
    "../closure/closure.c"
  }

project "set"
  kind "StaticLib"
  language "C"
  links { "closure" }
  includedirs { "../closure" }
  files {
    "../set/set.c"
  }

project "future"
  kind "StaticLib"
  language "C"
  links { "closure", "set" }
  buildoptions { "-Wno-deprecated-declarations", "-std=gnu11" }
  includedirs { "../closure", "../set", "src/sched" }
  files {
    "../future/future.c",
    "../future/tit_eager.c",
    "../future/tit_lazy.c"
  }

