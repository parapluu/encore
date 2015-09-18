def sample(args)
  path=File.join(File.dirname(__FILE__), "../programs/pl_shootout/fannkuch/")
  threads = ENV['ponythreads'] || 4
  raise ArgumentError, "too few arguments" if args.length < 1
  n = args[0]
  cmd = "./fannkuch_foreach --ponythreads #{threads} #{n}"
  execute(path, cmd)
end

def expected(args)
  n = args[0]
  result = case n
           when 0 then 0
           when 1 then 0
           when 2 then 1
           when 3 then 2
           when 4 then 4
           when 5 then 7
           when 6 then 10
           when 7 then 16
           when 8 then 22
           when 9 then 30
           when 10 then 38
           when 11 then 51
           when 12 then 65
           when 13 then 80
           else -1
           end
  if result == -1
    return "Pfannkuchen(#{n})" # result unkown
  else
    "Pfannkuchen(#{n}) = ".concat(result.to_s)
  end
end
