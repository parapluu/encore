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
  
  err = false
  missingSubstrings = []
  if expectedOutput.is_a?(String)
    if programOutput.include? expectedOutput 
      puts "#{result}"
    else
      puts "Measurement failed due to erroneous program output.\n"\
      "Program output:\n#{programOutput}\n"\
      "Expected to contain substring:\n#{expectedOutput}"
    end
  else
    expectedOutput.each do |substring|
      if not programOutput.include? substring
        missingSubstrings.push(substring)
        err = true
      end
    end
    if not err
      puts "#{result}"
    else
      missingOutput = ""
      missingSubstrings.each do |substring|
        missingOutput.concat(substring).concat("\n")
      end
      puts "Measurement failed due to erroneous program output.\n"\
      "Program output:\n#{programOutput}\n"\
      "Expected to contain substring:\n#{missingOutput}"
    end
  end
end
