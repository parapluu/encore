# Dependency checker module that checks for missing dependencies

def checkMissingDependencies(commands)
  missing = []
  commands.each do |command|
    if not system("which #{ command} > /dev/null 2>&1")
      missing.push(command)
    end
  end
  missing
end

def printMissingDependencies(deps)
  puts "The following dependencies were missing:\n"  
  deps.each do |dep|
    puts dep
  end
end

def checkOS()
  RUBY_PLATFORM
end

def checkDependencies()
  missing = checkMissingDependencies(DEPENDENCIES)
  if not missing.empty?
    return true, missing
  end

  current_os = checkOS()
  if not OS_SUPPORT.any? { |os| current_os.include? os}
    return true, OS_SUPPORT
  end
  return false
end
