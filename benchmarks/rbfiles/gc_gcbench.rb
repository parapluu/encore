def sample(args)
  path=File.join(File.dirname(__FILE__), "../programs/GCBench/") # path to program
  threads = ENV['ponythreads'] || 2 # ponythreads value is stored in
  # the 'ponythreads' environment
  # variable
  command = "./GCBench --ponythreads #{threads}"
  execute(path, command)
end

def expected(args)
  return ["Done!",
          "Stretch: 2097151",
          "Long lived: 1048575",
          "524288 trees of depth 6 size: 33030144",
          "32 trees of depth 20 size: 33554400",
          "131072 trees of depth 8 size: 33423360",
          "128 trees of depth 18 size: 33554304",
          "32768 trees of depth 10 size: 33521664",
          "512 trees of depth 16 size: 33553920",
          "2048 trees of depth 14 size: 33552384",
          "8192 trees of depth 12 size: 33546240",
          "2097152 trees of depth 4 size: 31457280"]
  end
