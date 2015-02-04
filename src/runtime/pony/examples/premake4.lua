function c_app()
  kind "ConsoleApp"
  links "pony"
  
  configuration "not windows"
    language "C"
    buildoptions "-std=gnu11"

  configuration "*"
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
  files {
    "RandomAccess/*.c",
    "RandomAccess/*.h"
  }

project "gups2"
  c_app()
  files {
    "RandomAccess2/*.c",
    "RandomAccess2/*.h"
  }

project "dist"
  c_app()
  files "dist.c"
