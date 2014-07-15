solution "ponyrt"
  configurations {"Debug", "Release"}

  links { "future", "set", "closure" }

  buildoptions {
    "-mcx16",
    "-pthread",
    "-march=native"
    }

  linkoptions {
    "-lm",
    "-pthread"
    }

  includedirs {
    "inc/"
    }

  flags {
    "ExtraWarnings",
    "FatalWarnings",
    "Symbols"
    }

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

  include("src/")
  include("examples/")
  include("utils/")
  include("test/")
