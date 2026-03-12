module Ectl

  def self.sessions
    return @sessions if defined? @sessions
    @sessions = {}
    Dir.glob("/tmp/emacs*/*").each do |f|
      s = Daemon.new f
      raise "Internal deficiency" if @sessions[s.name] # TODO
      @sessions[s.name] = s
    end
    @sessions
  end


  class Daemon

    attr_reader :name

    def initialize filename # TODO split filename by regex properly
      @file = filename
      @name = @file.sub %r{^/tmp/emacs[^/]*/}, ""
      @pid = nil
      # TODO make use of @uid
    end

    def pid
      cmd = "lsof -t #{@file}"
      @pid ||= `#{cmd}`.chomp
      raise "Failed while executing '#{cmd}'" if $?.exitstatus != 0
      @pid
    end

    def kill
      cmd = "kill #{pid}"
      $log.info cmd
      `#{cmd}`
    end

  end

end
