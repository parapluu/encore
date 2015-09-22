def sample(args)
  path=File.join(File.dirname(__FILE__), "../programs/general/big/Scala")
  raise ArgumentError, "too few arguments" if args.length < 1
  workers = args[0]
  cmd = "scala big #{workers}"
  execute(path, cmd)
end
