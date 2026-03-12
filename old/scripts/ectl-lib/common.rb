require 'logger'

MYDIR = File.dirname(File.absolute_path $0)
MYNAME = $0.split('/')[-1]

module Ectl
  class PositiveResult < Exception;      end # premature successfull exit
  class NegativeResult < Exception;      end # negative result (e.g. not found)
  class GeneralFailure < RuntimeError;   end # top-level class for errors
  class InvalidUser    < GeneralFailure; end # invalid user command
  class InvalidSystem  < GeneralFailure; end # system error (e.g. r/w error)
  class InvalidProgram < GeneralFailure; end # internal error
end

# DEBUG < INFO < WARN < ERROR < FATAL < UNKNOWN
$log = Logger.new(STDERR)
$log.level = Logger::DEBUG

$log.formatter = proc do |severity, datetime, progname, msg|
  "#{msg}\n"
end

# reports result of the execution
def echo msg
  $stdout.puts msg
end

def bad_usage
  $log.error "Invalid usage"
  usage
  raise Ectl::UserInvalid, "Invalid usage"
end

