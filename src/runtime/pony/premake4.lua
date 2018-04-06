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

if os.execute("clang -v") == 0 then
  premake.gcc.cc  = 'clang'
  premake.gcc.cxx = 'clang++'
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
      "-Wunused-parameter",
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
    "../dtrace",
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

  configuration "not windows"
    if(table.contains(_ARGS, "dtrace")) then
      defines "USE_DYNAMIC_TRACE"
      os.execute("echo '#define USE_DYNAMIC_TRACE' > ../../../release/inc/dtrace_enabled.h")

      if os.execute("dtrace -h -s ../common/encore_probes.d -o ../common/encore_probes.h") ~= 0 then
        print("Error generating encore DTrace headers. Stop");
        os.exit(1)
      end

      if os.execute("dtrace -h -s ../common/dtrace_probes.d -o ../common/dtrace_probes.h") ~= 0 then
        print("Error generating ponyc DTrace headers. Stop");
        os.exit(1)
      end

      if os.is("linux") then
        os.execute("dtrace -G -s ../common/encore_probes.d -o ../common/encore_probes.o")
        os.execute("dtrace -G -s ../common/dtrace_probes.d -o ../common/dtrace_probes.o")

        project "ponyrt"
          configuration "Debug"
            postbuildcommands {
              'ar -rcs bin/debug/libponyrt.a ../common/encore_probes.o',
              'ar -rcs bin/debug/libponyrt.a ../common/dtrace_probes.o'
            }

          configuration "Release"
            postbuildcommands {
              'ar -rcs bin/release/libponyrt.a ../common/encore_probes.o',
              'ar -rcs bin/release/libponyrt.a ../common/dtrace_probes.o',
            }
      end
    else
      os.execute("cat /dev/null > ../../../release/inc/dtrace_enabled.h")
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
    "../party/**.h",
    "../party/**.c"
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
