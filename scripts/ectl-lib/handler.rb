require 'getoptlong'
require_relative 'common.rb'

module Ectl

  Handlers = []

  class Handler

    def self.inherited kl
      Handlers << kl.new
    end

    def full_name
      format "%-30s", "#{MYNAME} #{name}"
    end

  end


  class List < Handler

    def name
      "ls"
    end

    def usage
      "#{full_name} -- list existing sessions\n"
    end

    def run
      bad_usage unless ARGV.empty?
      Ectl.sessions.keys.sort.each { |s| echo s }
    end

  end


  class Attach < Handler

    def name
      "attach"
    end

    def usage
      "#{full_name} -- attach to existing session\n"
    end

    def run

      create = nil

      opts = GetoptLong.new(
          [ '--create', '-c', GetoptLong::NO_ARGUMENT ],
      )

      opts.each do |opt, arg|
        case opt
        when '--create'
          create = true
        else
          bad_usage
          exit 1
        end
      end

      bad_usage unless ARGV.size == 1

      sesname = ARGV[0]

      unless Ectl.sessions[sesname]
        if create
          $log.info "Creating new session '#{sesname}'"
          # TODO move elisp-code into emacs files
          system "emacs", "--daemon=#{sesname}", "--eval", \
              %Q{(setq frame-title-format (quote (multiple-frames ("#{sesname} :: %b") ("" "#{sesname} :: %b"))))}
              #%Q{(setq frame-title-format (quote (multiple-frames ("#{sesname} :: %b") ("" "#{sesname}"))))}
          unless $?.exitstatus == 0
            $log.error "Failed to start daemon"
            raise InvalidSystem 
          end
        else
          $log.error "No session named '#{sesname}' found"
          raise InvalidUser
        end
      end

      exec "emacsclient", "--no-wait", "-s", sesname, "-c"

    end

  end

  
  class Kill < Handler
    
    def name
      "kill"
    end

    def usage
      "#{full_name} -- kill session\n"
    end

    def run
      if ARGV.empty?
        $log.warning "No sessions to kill"
        return nil
      end

      all = Ectl.sessions
      ARGV.each do |s|
        one = all[s]
        unless one
          $log.error "Cannot find session '#{s}'"
          next
        end
        $log.info "Killing session '#{s}'"
        one.kill
      end

    end

  end

end
