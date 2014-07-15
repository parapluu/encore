function unittest()
  configuration "Debug"
    targetdir "../bin/tests/debug"
    objdir "../obj/tests/debug"

  configuration "Release"
    targetdir "../bin/tests/release"
    objdir "../obj/tests/release"

  configuration "macosx"
    links { "c++" }

  configuration "*"
    includedirs {
      "../utils/gtest/",
      "../src/",
      "../inc/"
    }
    libdirs "../bin/utils/"
    buildoptions {
      "-std=gnu++11",
      "-mcx16",
      "-pthread",
      "-march=native"
      }
    language "C++"
    kind "ConsoleApp"
    links { "gtest", "pony", "future", "set", "closure" }
    linkoptions {
      "-pthread"
      }
end

project "ds"
  unittest()
  files "ds/*.cc"

project "mem"
  unittest()
  files "mem/*.cc"

