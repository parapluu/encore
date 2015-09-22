def sample(args)
  path=File.join(File.dirname(__FILE__), "../programs/pl_shootout/fannkuch/Erlang-Hipe")
  raise ArgumentError, "too few arguments" if args.length < 1
  n = args[0]
  cmd = "erl -smp enable -noshell -run fannkuchredux main #{n}"
  execute(path, cmd)
end
