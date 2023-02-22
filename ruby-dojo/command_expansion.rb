# Both backticks and %x{} will run OS commands!
puts `date`

puts "Non-Ruby files:"
files = %x{ls}.split("\n").reject { |line| line =~ /\.rb/ }
files.each { |file| puts "  - #{file}" }
puts $? # PID and exit code
