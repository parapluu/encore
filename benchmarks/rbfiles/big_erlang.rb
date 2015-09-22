def sample(args)
  path=File.join(File.dirname(__FILE__), "../programs/general/big/Erlang-Hipe")
  raise ArgumentError, "too few arguments" if args.length < 1
  workers = args[0]
  cmd = "erl -smp enable -noshell -run big  main #{workers}"
  execute(path, cmd)
end
