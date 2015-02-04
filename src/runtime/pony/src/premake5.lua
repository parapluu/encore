project "ponyrt"
  targetname "pony"
  language "C"
  kind "StaticLib"
  includedirs {
    "../inc/"
  }
  files {
    "../inc/**.h",
    "**.h",
    "**.c"
  }
  configuration "gmake"
    buildoptions {
      "-std=gnu11"
    }
  configuration "vs*"
    cppforce { "**.c" }
