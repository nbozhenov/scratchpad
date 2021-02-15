#!/usr/bin/env ruby
#coding: utf-8
require 'shellwords'

MULTTIME = [
    # [ "08:30", "09:30" ],
    # [ "18:00", "19:30" ],
    # [ "18:30", "21:00" ],
    # [ "17:00", "21:30" ],
    # [ "10:00", "21:30" ],
]

TARGETDIR = "/home/artur-share/Mult"
DESKICONS = [
    "/home/artur/Desktop/Mult.desktop",
    "/home/artur/Desktop/GoldMiner.desktop",
    "/home/artur/Desktop/Ветка.desktop"
]

unless defined? File.absolute_path
  def File.absolute_path abc
    abc
  end
end

MYDIR = File.dirname(File.absolute_path $0)
MYNAME = $0.split('/')[-1]

def floattime h, m
  Float(h) + Float(m)/60
end

def timenow
  t = Time.now
  floattime t.hour, t.min
end

def string2float str
  m = str.match /^([[:digit:]]+):([[:digit:]]{2})$/
  floattime m[1], m[2]
end

def MULTTIME.include? time
  retval = catch :yes do
    each do |rg|
      start = string2float rg[0]
      finish = string2float rg[1]
      if time >= start and time < finish
        throw :yes, true
      end
    end
    nil
  end
  if retval
      puts "it includes"
  else
      puts "it doesn't include"
  end
  retval
end

def allowed?
  retval = true
  begin
      Dir.entries(TARGETDIR)
  rescue Errno::EACCES
      retval = false
  end
  if retval
      puts "it's allowed"
  else
      puts "it's denied"
  end
  retval
end

def allow
  system "chmod", "-R", "a+r", TARGETDIR
  DESKICONS.each { |i| `chmod o+x #{i}` }
end

def deny
  list = `find #{TARGETDIR.shellescape} -print0`.split("\0")
  list.each do |f|
    system "chmod", "a-r", f
  end
  DESKICONS.each { |i| `chmod o-x #{i}` }
  system "#{MYDIR}/killall-suid vlc smplayer mplayer firefox chromium amarok dosbox"
end

access = allowed?
should = MULTTIME.include? timenow

if access and not should
  puts "should deny"
  deny
elsif not access and should
  puts "should allow"
  allow
end
