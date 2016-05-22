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

function use_flto()
  buildoptions {
    "-O3",
    "-flto",
  }
  linkoptions {
    "-flto",
    "-fuse-ld=gold",
  }
end

function c_lib()
  kind "StaticLib"
  language "C"

  configuration "not windows"
    buildoptions "-std=gnu11"

  configuration "Release or Profile"
    if(macosx or linux) then
      use_flto()
    else
      if(optimize) then
        optimize "Speed"
      else
        flags "OptimizeSpeed"
      end
    end

  configuration "*"
end

solution "ponyrt"
  configurations {"Debug", "Release"}

  configuration "not windows"
    buildoptions {
      "-mcx16",
      "-pthread",
      "-std=gnu11",
      "-march=native",

      "-Wno-error=deprecated-declarations",
      }

    linkoptions {
      "-lm",
      "-pthread"
      }

  configuration "*"

  includedirs {
    "../closure",
    "../array",
    "../encore",
    "../future",
    "../task",
    "../adt",
    "../party",
    "libponyrt",
    "../common",
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

project "ponyrt"
  c_lib()
  includedirs {
    "./libponyrt/",
  }
  files {
    "./libponyrt/**.h",
    "./libponyrt/**.c"
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

project "tuple"
  c_lib()
  files {
    "../tuple/tuple.h",
    "../tuple/tuple.c"
  }

project "range"
  c_lib()
  files {
    "../range/range.h",
    "../range/range.c"
  }

project "task"
  c_lib()
  files {
    "../task/task.h",
    "../task/task.c"
  }

project "optiontype"
  c_lib()
  links { "encore" }
  files {
    "../adt/option.h",
    "../adt/option.c"
  }

project "closure"
  c_lib()

  files {
    "../closure/closure.h",
    "../closure/closure.c"
  }

project "party"
  c_lib()
  links { "future", "array" }

  files {
    "../party/party.*",
  }

project "future"
  c_lib()
  links { "closure", "array" }
  files {
    "../future/future.c",
  }

project "stream"
  c_lib()
  links { "future" }
  files {
    "../stream/stream.h",
    "../stream/stream.c"
  }

-- -- project "set"
-- --   kind "StaticLib"
-- --   language "C"
-- --   links { "closure" }
-- --   includedirs { "../closure" }
-- --   files {
-- --     "../set/set.c"
-- --   }
