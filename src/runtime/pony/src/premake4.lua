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

project "pony"
  c_lib()
  includedirs {
    "../inc/"
  }
  files {
    "../inc/**.h",
    "**.h",
    "**.c"
  }
