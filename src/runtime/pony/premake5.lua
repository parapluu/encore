-- The Pony Runtime System
-- <URL_HERE>
-- <LICENSE HERE>

  solution "ponyrt"
    configurations { "Debug", "Release", "Profile" }
    location( _OPTIONS["to"] )

    flags {
      "FatalWarnings",
      "MultiProcessorCompile",
      "StaticRuntime"
    }

    configuration "Debug"
      targetdir "bin/debug"
      objdir "bin/debug"
      defines "DEBUG"
      flags { "Symbols" }

    configuration "Release"
      targetdir "bin/release"
      objdir "obj/release"

    configuration "Profile"
      targetdir "bin/profile"
      objdir "obj/profile"

    configuration "Release or Profile"
      defines "NDEBUG"
      optimize "Speed"
      flags { "LinkTimeOptimization" }

      if not os.is("windows") then
        linkoptions {
          "-flto",
          "-fuse-ld=gold"
        }
      end

    configuration { "Profile", "gmake" }
      buildoptions "-pg"
      linkoptions "-pg"

    --TODO profile build Visual Studio
    --configuration { "Profile", "vs*" }
    --  linkoptions "/PROFILE"

    configuration "not windows"
      linkoptions { "-pthread" }

    configuration { "macosx", "gmake" }
      toolset "clang"
      buildoptions "-Qunused-arguments"
      linkoptions "-Qunused-arguments"

    configuration "gmake"
      buildoptions {
        "-mcx16",
        "-march=native"
      }

    configuration "vs*"
      architecture "x64"
    configuration "*"

  dofile("scripts/properties.lua")
  include("src/")

if( _OPTIONS["with-tests"] or _OPTIONS["run-tests"] ) then
  project "gtest"
    targetname "gtest"
    language "C++"
    kind "StaticLib"
    includedirs {
      "utils/gtest"
    }
    files {
      "utils/gtest/gtest-all.cc"
    }

  project "tests"
    targetname "tests"
    language "C++"
    kind "ConsoleApp"
    includedirs {
      "inc/",
      "src/",
      "utils/gtest"
    }
    files {
      "test/**.cc"
    }
    configuration "gmake"
      buildoptions {
        "-std=gnu++11"
      }
    configuration "*"
    links {
      "ponyrt",
      "gtest"
    }
    if( _OPTIONS["run-tests"] ) then
      configuration "gmake"
        postbuildcommands { "$(TARGET)" }
      configuration "vs*"
        postbuildcommands { "\"$(TargetPath)\"" }
    end
end

if( _OPTIONS["with-examples"] ) then
  local function example_app()
    language "C"
    kind "ConsoleApp"
    configuration "gmake"
      buildoptions { "--std=gnu11" }
    configuration "*"
    links { "ponyrt" }
  end

  project "counter"
    targetname "counter"
    example_app()
    includedirs { "inc/" }
    files {
      "examples/counter.c"
    }
    configuration "vs*"
      cppforce { "examples/counter.c" }
    configuration "*"

  project "mailbox"
    targetname "mailbox"
    example_app()
    includedirs { "inc/" }
    files {
      "examples/mailbox.c"
    }
    configuration "vs*"
      cppforce { "examples/mailbox.c" }
    configuration "*"

  project "mixed"
    targetname "mixed"
    example_app()
    includedirs { "inc/" }
    files {
      "examples/mixed.c"
    }
    configuration "vs*"
      cppforce { "examples/mixed.c" }
    configuration "*"

  project "ring"
    targetname "ring"
    example_app()
    includedirs { "inc/" }
    files {
      "examples/ring.c"
    }
    configuration "vs*"
      cppforce { "examples/ring.c" }
    configuration "*"

  project "service"
    targetname "service"
    example_app()
    includedirs { "inc/" }
    files {
      "examples/service.c"
    }
    configuration "vs*"
      cppforce { "examples/service.c" }
    configuration "*"

  project "spreader"
    targetname "spreader"
    example_app()
    includedirs { "inc/" }
    files {
      "examples/spreader.c"
    }
    configuration "vs*"
      cppforce { "examples/spreader.c" }
    configuration "*"

  project "gups"
    targetname "gups"
    example_app()
    includedirs { "inc/" }
    files {
      "examples/RandomAccess/*.c",
      "examples/RandomAccess/*.h"
    }
    configuration "vs*"
      cppforce { "examples/RandomAccess/*.c" }
    configuration "*"

  project "gups2"
    targetname "counter"
    example_app()
    includedirs { "inc/" }
    files {
      "examples/RandomAccess2/*.c",
      "examples/RandomAccess2/*.h"
    }
    configuration "vs*"
      cppforce { "examples/RandomAccess2/*.c" }
    configuration "*"
end

  if _ACTION == "clean" then
    os.rmdir("bin")
    os.rmdir("obj")
  end

  -- Allow for out-of-source builds.
  newoption {
    trigger     = "to",
    value       = "path",
    description = "Set output location for generated files."
  }

  newoption {
    trigger     = "with-tests",
    description = "Compile test suite for every build."
  }

  newoption {
    trigger     = "with-examples",
    description = "Compile example programs for every build."
  }

  newoption {
    trigger = "run-tests",
    description = "Run the test suite on every successful build."
  }

  newoption {
    trigger     = "use-docsgen",
    description = "Select a tool for generating the API documentation",
    allowed = {
      { "sphinx", "Chooses Sphinx as documentation tool. (Default)" },
      { "doxygen", "Chooses Doxygen as documentation tool." }
    }
  }

  dofile("scripts/release.lua")

  -- Package release versions of Pony for all supported platforms.
  newaction {
    trigger     = "release",
    description = "Prepare a new Pony release.",
    execute     = dorelease
  }

  dofile("scripts/docs.lua")

  newaction {
    trigger     = "docs",
    value       = "tool",
    description = "Produce API documentation.",
    execute     = dodocs
  }
