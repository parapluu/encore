function c_app()
  kind "ConsoleApp"
  language "C"
  links { "pony", "future", "set", "closure" }
  buildoptions "-std=gnu11"
  buildoptions {
    "-mcx16",
    "-pthread",
    "-march=native"
    }

  linkoptions {
    "-lm",
    "-pthread"
    }

  flags {
    "ExtraWarnings",
--    "FatalWarnings",
    "Symbols"
    }

end

project "service"
  c_app()
  files "service.c"

project "spreader"
  c_app()
  files "spreader.c"

project "counter"
  c_app()
  files "counter.c"

project "ring"
  c_app()
  files "ring.c"

project "mailbox"
  c_app()
  files "mailbox.c"

project "mixed"
  c_app()
  files "mixed.c"

project "gups"
  c_app()
  files "RandomAccess/*.c"

project "gups2"
  c_app()
  files "RandomAccess2/*.c"

project "fut-eager"
  c_app()
  includedirs { 
    "../../future", 
    "../../closure", 
    "../../set"
  }
  files {
    "fut/eager.c"
  } 

project "fut-lazy"
  c_app()
  includedirs { 
    "../../future", 
    "../../closure", 
    "../../set"
  }
  files {
    "fut/lazy.c"
  }

project "fut-shared"
  c_app()
  includedirs {
    "../../future",
    "../../closure",
    "../../set"
  }
  files {
    "fut/shared.c"
  }

