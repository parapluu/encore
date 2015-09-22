# -*- coding: utf-8 -*-
require "optparse"

require_relative "config"

ENV['ponythreads'] = PONYTHREADS_DEFAULT.to_s
@tag = ""
@silent = SILENT
@verbose = VERBOSE

# TODO: rewrite recursively (looks nicer)
def fastest(t1, t2)
  m1, s1, f1 = t1[0], t1[2..3], t1[5..6]
  m2, s2, f2 = t2[0], t2[2..3], t2[5..6]
  
  if m1 < m2
    t1
  elsif m1 > m2
    t2
  else
    if s1 < s2
      t1
    elsif s1 > s2
      t2
    else
      if f1 < f2
        t1
      elsif f1 > f2
        t2
      else t1
      end
    end
  end
end

# given arr[el1, el2, el3] returns string on form "el1 el2 el3 "
def commandParser(commandList)
  commands = ""
  commandList.each do |command|
    commands = commands + command.to_s + " "
  end
  commands
end

def run(benchmarks)
  benchmarks.each do |benchmark, configuration|
    if not @silent
      puts "running #{benchmark} benchmark"
    end
    configuration.each do |name, arguments|
      next if IGNORES.include? [name, benchmark]

      input = commandParser(arguments)
      dataFile = if @tag.empty? 
                   "data/#{benchmark}-#{name}.data"
                 else
                   "data/#{@tag}-#{benchmark}-#{name}.data"
                 end

      if not @silent
        puts("\texecuting with arguments #{arguments} #{REPS} times")
        if @verbose
          puts "\truby -I#{WD} -I#{WD}/rbfiles -r #{benchmark} -r #{WD}/helper.rb #{WD}/runner.rb #{input}"
        end
      end 
      data = `ruby -I#{WD} -I#{WD}/rbfiles -r #{benchmark} -r #{WD}/helper.rb #{WD}/runner.rb #{input}`

      samples = []
      times = []
      memory = []
      
      avgMem = 0

      data.lines.each do |line|
        matchtime = line.match(/(\d+\:.\d+\.\d+)/)
        matchmemory = line.match(/Maximum resident set size .+ (\d+)/)
        if matchtime
          time = matchtime[1]
          samples.push(matchtime[1])
        end
        
        memory.push(matchmemory[1]) if matchmemory
      end

      fastestTime = samples[0] if samples[0]

      # Calculates lowest elapsed time
      samples.each do |t|
        if fastest(t, fastestTime) == t
          fastestTime = t
        end
      end

      # Calculates average memory usage
      memory.each do |m|
        avgMem = avgMem + m.to_i
      end
      avgMem = avgMem / memory.size if memory.size > 0

      File.open(dataFile, "w") do |file|
        if not @silent
          puts("\twrote result to #{dataFile}")
        end
        file.write(data) # TODO: put summary at top of file?
        file.write("\nSummary:\n")
        file.write("Fastest time: #{fastestTime}\n")
        file.write("Average memory usage: #{avgMem}\n")
      end
      if not @silent
        puts ""
      end
    end
  end
end

opts = OptionParser.new do |o| 
  o.banner = "Usage: ruby bench.rb [options]"

  o.on("-p", "--ponythreads [THREADS]", "Specifies the amount of ponythreads to use when executing encore programs") do |t| 
    ENV['ponythreads'] = t
  end

  o.on("-t", "--tag [TAG]", "Prepends a tag to the generated data files") do |t|
    @tag = t
  end

  o.on("-s", "--silent", "runs benchmarks without console output") do
    @silent = true
  end

  o.on("-v", "--verbose", "runs benchmarks with verbose output") do
    @verbose = true
  end
end
opts.parse!

run(BENCHMARKS)

