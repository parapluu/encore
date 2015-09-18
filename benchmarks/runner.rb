# Everything that this file prints is included in the generated datafile
require_relative "config"

if DATE
  puts Time.now
end

if THREADS
  puts "ponythreads: #{ENV['ponythreads']}"
end

REPS.times do
  result, programOutput = sample(ARGV)
  if INCLUDE_OUTPUT
    result.concat("\nProgram output:\n#{programOutput}")
  end

  expectedOutput = if defined? expected
                     expected(ARGV) 
                   else
                     ""
                   end

  puts "-------"
  if programOutput.include? expectedOutput 
    puts "#{result}"
  else
    puts "Measurement failed due to erroneous program output.\nOutput:\n#{programOutput}\nExpected to contain substring:\n#{expectedOutput}"
  end
end
