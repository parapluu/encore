def sample(args)
  path=File.join(File.dirname(__FILE__), "../programs/pl_shootout/chameneos/Scala")
  raise ArgumentError, "too few arguments" if args.length < 1
  meetings, amount = *args # ignores other values in args except the first two
  cmd = "scala chameneosredux #{meetings} #{amount}"
  execute(path, cmd)
end
