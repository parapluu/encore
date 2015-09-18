require 'open3'

require_relative "config"

def execute(path, command)
  cmd = "cd #{path}; #{TIME} #{command}"

  timeMeasurements = ""
  programOutput = ""

  # TODO: check return code of program
  Open3.popen3(cmd) do |stdin, stdout, stderr, wait_thr|
    timeMeasurements = stderr.read
    programOutput = stdout.read
  end

  return timeMeasurements, programOutput
end
