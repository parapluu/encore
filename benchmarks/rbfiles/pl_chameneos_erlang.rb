def sample(args)
  path=File.join(File.dirname(__FILE__), "../programs/pl_shootout/chameneos/Erlang-Hipe")
  raise ArgumentError, "too few arguments" if args.length < 1
  meetings, amount = *args # ignores other values in args except the first two
  cmd = "erl -smp enable -noshell -run  chameneos_redux main #{meetings} #{amount}"
  execute(path, cmd)
end
