-- premake.override is a premake5 feature.
-- We use this approach to improve Visual Studio Support for Windows.
-- The goal is to force C++ compilation for non-*.cpp/cxx/cc file extensions.
-- By doing this properly, we avoid a MSVC warning about overiding /TC
-- with /TP.

if premake.override then
  local force_cpp = { }

  function cppforce(inFiles)
    for _, val in ipairs(inFiles) do
      for _, fname in ipairs(os.matchfiles(val)) do
        table.insert(force_cpp, path.getabsolute(fname))
      end
    end
  end

  premake.override(premake.vstudio.vc2010, "additionalCompileOptions", function(base, cfg, condition)
    if cfg.abspath then
      if table.contains(force_cpp, cfg.abspath) then
        _p(3,'<CompileAs %s>CompileAsCpp</CompileAs>', condition)
      end
    end
    return base(cfg, condition)
  end)
end

solution "ponyrt"
  configurations {"Debug", "Release"}

  configuration "not windows"
    buildoptions {
      "-mcx16",
      "-pthread",
      "-std=gnu11",
      "-fms-extensions",
      "-march=native"
      }

    linkoptions {
      "-lm",
      "-pthread"
      }

  configuration "*"

  includedirs {
    "inc/",
    "../closure",
    "../set",
    "../encore",
    "../future",
    "src/sched"
  }

  flags {
    "ExtraWarnings",
--    "FatalWarnings",
    "Symbols"
    }

  configuration "Debug"
    targetdir "bin/debug"
    objdir "obj/debug"

  configuration "Release"
    targetdir "bin/release"
    objdir "obj/release"
    defines "NDEBUG"

  configuration "macosx"
    -- set these to suppress clang warning about -pthread being unused
    buildoptions "-Qunused-arguments"
    linkoptions "-Qunused-arguments"

  configuration "windows"
    if(architecture) then
      architecture "x64"
    end

    linkoptions "/NODEFAULTLIB:msvcrt.lib"

    if(cppforce) then
      cppforce { "**.c" }
    end

  include("src/")
  -- include("examples/")
  include("utils/")
  -- include("test/")

project "closure"
  c_lib()
  files {
    "../closure/closure.h",
    "../closure/closure.c"
  }

project "encore"
  c_lib()
  files {
    "../encore/encore.h",
    "../encore/encore.c"
  }

project "array"
  c_lib()
  files {
    "../array/array.h",
    "../array/array.c"
  }

project "stream"
  c_lib()
  links { "future" }
  files {
    "../stream/stream.h",
    "../stream/stream.c"
  }

-- project "set"
--   kind "StaticLib"
--   language "C"
--   links { "closure" }
--   includedirs { "../closure" }
--   files {
--     "../set/set.c"
--   }

project "future"
  c_lib()
  links { "closure", "set" }
  buildoptions {
      "-Wno-deprecated-declarations",
  }
  files {
    "../future/future.c",
  }
