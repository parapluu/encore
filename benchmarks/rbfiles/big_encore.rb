def sample(args)
  path=File.join(File.dirname(__FILE__), "../programs/general/big/") # path to program
  threads = ENV['ponythreads'] || 2
  raise ArgumentError, "too few arguments" if args.length < 1 # optional
  workers = args[0]
  command = "./big --ponythreads #{threads} #{workers}"
  execute(path, command)
end

def expected(args)
  return "Everything should be done now!"
end
