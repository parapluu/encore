def sample(args)
  path=File.join(File.dirname(__FILE__), "../programs/pl_shootout/chameneos/")
  threads = ENV['ponythreads'] || 2
  raise ArgumentError, "too few arguments" if args.length < 1
  meetings, amount = *args # ignores other values in args except the first two
  cmd = "./chameneos_redux --ponythreads #{threads} #{meetings} #{amount}"
  execute(path, cmd)
end

# Auxilliary function to spell
def lookup(num) 
  case num
  when 0
    "zero"
  when 1
    "one"
  when 2
    "two"
  when 3
    "three"
  when 4
    "four"
  when 5
    "five"
  when 6
    "six"
  when 7
    "seven"
  when 8
    "eight"
  when 9
    "nine"
  end
end

# Auxilliary function to expected
def spell(n)
  result = ""
  remaining = n
  
  while (remaining > 0) 
    result.insert(0, lookup(remaining % 10).concat(" "))
    remaining = remaining / 10
  end
  
  return result
end

# This function only works correctly for the creature parameterized version
def expected(args)
  meetings = args[0]
  spell(meetings.to_i * 2)
end
