function c_lib()
  kind "StaticLib"
  language "C"
  buildoptions "-std=gnu11"
end

project "pony"
  c_lib()
  files "**.c"
