def sample(args)
  path=File.join(File.dirname(__FILE__), "../programs/pl_shootout/fannkuch/Scala")
  raise ArgumentError, "too few arguments" if args.length < 1
  n = args[0]
  cmd = "scala fannkuchredux #{n}"
  execute(path, cmd)
end
