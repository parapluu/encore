# example rb file, this one executes the encore version of chameneos

def sample(args)
  path=File.join(File.dirname(__FILE__), "../programs/pl_shootout/chameneos/") # path to program
  threads = ENV['ponythreads'] || 2 # ponythreads value is stored in
                                    # the 'ponythreads' environment
                                    # variable
  raise ArgumentError, "too few arguments" if args.length < 1 # optional
  meetings, amount = *args # ignores other values in args except the
                           # first two. If only one value is present,
                           # the value is bound to meetings and amount
                           # is left empty.
  command = "./chameneos_redux --ponythreads #{threads} #{meetings} #{amount}"
  execute(path, command)
end
