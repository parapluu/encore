use_clang_on_osx = true
-- use_arch = "corei7-avx"
use_arch = "native"

solution "ponyrt"
  configurations {
    "Debug",
    "Release"
    }

  includedirs { "inc/", "../set/", "../future/", "../closure/", "src/" }

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
--      "-flto=jobserver",
      "-fuse-ld=gold",
--      "-fwhole-program",
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
--    if use_clang_on_osx then
--      buildoptions "-O4"
--    else
      buildoptions "-O3"
--    end

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
    buildoptions { "-Wno-deprecated-declarations" }
    files {
      "src/*.c",
      "../future/future.c",
      "../future/future_actor.c",
      "../future/tit_eager.c",
      "../future/tit_lazy.c",
      "../set/set.c",
      "../closure/closure.c"
    }
    -- links {
    --   "Futures"
    -- }

  -- project "Futures"
  --   kind "StaticLib"
  --   language "C"
  --   links { "Closures", "Set" }
  --   buildoptions { "-Wno-deprecated-declarations" }
  --   files {
  --       "../future/future.c",
  --       "../future/future_actor.c",
  --       "../future/tit_eager.c",
  --       "../future/tit_lazy.c"
  --   }

  -- project "Closures"
  --   kind "StaticLib"
  --   language "C"
  --   files {
  --       "../closure/closure.c"
  --   }

  -- project "Set"
  --   kind "StaticLib"
  --   language "C"
  --   files {
  --       "../set/set.c"
  --   }

  project "spreader"
    kind "ConsoleApp"
    language "C"
    links { "pony" }
    files "test/spreader.c"

  project "counter"
    kind "ConsoleApp"
    language "C"
    links { "pony" }
    files "test/counter.c"

  project "ring"
    kind "ConsoleApp"
    language "C"
    links { "pony" }
    files "test/ring.c"

  project "mailbox"
    kind "ConsoleApp"
    language "C"
    links { "pony" }
    files "test/mailbox.c"

  project "mixed"
    kind "ConsoleApp"
    language "C"
    links { "pony" }
    files "test/mixed.c"

  project "gups"
    kind "ConsoleApp"
    language "C"
    links { "pony" }
    files "test/RandomAccess/*.c"

  project "gups2"
    kind "ConsoleApp"
    language "C"
    links { "pony" }
    files "test/RandomAccess2/*.c"

  project "future_test_eager"
    kind "ConsoleApp"
    language "C"
    links { "pony" }
    files "test/future_test_eager/*.c"

  project "future_test_lazy"
    kind "ConsoleApp"
    language "C"
    links { "pony" }
    files "test/future_test_lazy/*.c"

