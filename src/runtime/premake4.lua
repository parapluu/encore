use_clang_on_osx = true
-- use_arch = "corei7-avx"
use_arch = "native"

solution "ponyrt"
  configurations {
    "Debug",
    "Release"
    }

  includedirs { "inc/" }

  buildoptions {
    "-std=gnu99",
    "-mcx16",
    "-pthread",
    "-march=" .. use_arch
    }

  linkoptions "-lm -pthread"

  flags {
    "ExtraWarnings",
    "FatalWarnings",
    "Symbols"
    }

  configuration "Debug"
    targetdir "bin/debug"

  configuration "Release"
    targetdir "bin/release"
    buildoptions "-flto"
    linkoptions {
      "-flto=jobserver",
      "-fuse-ld=gold",
      "-fwhole-program",
      }

  configuration {
    "Release",
    "not macosx"
    }
    buildoptions "-O3"

  configuration {
    "Release",
    "macosx"
    }
    if use_clang_on_osx then
      buildoptions "-O4"
    else
      buildoptions "-O3"
    end

  configuration "macosx"
    if use_clang_on_osx then
      buildoptions "-Qunused-arguments"
      linkoptions "-Qunused-arguments"
    else
      buildoptions {
        "-Wa,-q",
        "-Wa,-Wno-trigraphs"
        }
    end

  project "pony"
    kind "StaticLib"
    language "C"
    files "src/*.c"

  project "spreader"
    kind "ConsoleApp"
    language "C"
    links "pony"
    files "test/spreader.c"

  project "counter"
    kind "ConsoleApp"
    language "C"
    links "pony"
    files "test/counter.c"

  project "ring"
    kind "ConsoleApp"
    language "C"
    links "pony"
    files "test/ring.c"

  project "mailbox"
    kind "ConsoleApp"
    language "C"
    links "pony"
    files "test/mailbox.c"

  project "mixed"
    kind "ConsoleApp"
    language "C"
    links "pony"
    files "test/mixed.c"

  project "gups"
    kind "ConsoleApp"
    language "C"
    links "pony"
    files "test/RandomAccess/*.c"

  project "gups2"
    kind "ConsoleApp"
    language "C"
    links "pony"
    files "test/RandomAccess2/*.c"
