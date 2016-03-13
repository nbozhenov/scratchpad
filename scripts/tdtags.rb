#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

#
# ETAGS
#
# http://en.wikipedia.org/wiki/Ctags#Etags_2
#
# The etags files consists of multiple sections—one section per input
# source file. Sections are plain-text with several non-ascii
# characters used for special purposes. These characters are
# represented as bracketed hexadecimal codes below.
# 
# A section starts with a two line header, one line containing a
# single <\x0c> character, followed by a line which consists of:
# 
# {src_file},{size_of_tag_definition_data_in_bytes}
# 
# The header is followed by tag definitions, one definition per line,
# with the format:
# 
# {tag_definition_text}<\x7f>{tagname}<\x01>{line_number},{byte_offset}
# 
# {tagname} (along with <\x01>) can be omitted if the name of the tag
# can be deduced from the text at the tag definition.
#

require 'stringio'
#require_relative '../ruby/y-harness'

def enforce_line cond, line, lineno
  raise "Unknown line format:#{lineno}: '#{line.chomp}'" unless cond
end

Tag = Struct.new :name, :file, :line, :text, :offset

class Tags

  def initialize
    @files = {}
  end

  def [] key
    retval = @files[key]
    if retval.nil?
      @files[key] = Array.new
      retval = @files[key]
    end
    retval
  end

  def delete_if &block
    @files.each do |file, tags|
      tags.delete_if &block
    end
    @files.delete_if do |f, t|
      retval = false
      if t.empty?
        #$logger.debug "no more tags for '#{f}' (delete)"
        retval = true
      end
      retval
    end
  end

  def each_tag &block
    @files.each do |f, t|
      t.each &block
    end
  end

  def cappend input, overwrite
    counter = 0
    while line = input.gets
      counter += 1
      if line[0] == '!' or line.match /^operator[[:space:]]/
        #$logger.warn "Unsupported line format:#{counter}: '#{line.chomp}'"
        next
      end
      choppedline = line.sub /;".*$/, ""
      if line != choppedline
        #$logger.warn "Ignoring additional info:#{counter}: '#{line.chomp}'"
        line = choppedline
      end
      tokens = line.split
      enforce_line (tokens.size == 3), line, counter
      enforce_line (tokens[2] =~ /^[[:digit:]]*$/), line, counter
      tag = Tag.new
      tag.name = tokens[0]
      tag.file = tokens[1]
      tag.line = tokens[2].to_i
      if overwrite
        self[tag.file].delete_if do |t|
          t.line == tag.line
          t.name == tag.name
        end
      end
      self[tag.file] << tag
    end
  end

  def eappend input, overwrite # TODO: use overwrite flag
    counter = 0
    filename = nil
    stage = nil # :fstline, :sndline
    while line = input.gets
      counter += 1
      case stage
      when nil
        enforce_line (line == "\x0c\n"), line, counter
        stage = :fstline
      when :fstline
        match = line.match /^([^,]*),[^,]*$/
        enforce_line match, line, counter
        filename = match[1]
        stage = :sndline
        #$logger.debug "found section for file '#{filename}'"
      when :sndline
        if line == "\x0c\n"
          stage = :fstline
          filename = nil
        elsif line.include? "\x01"
          enforce_line (line =~ /^[^\x7f]*\x7f[^\x7f]*$/), line, counter
          enforce_line (line =~ /^[^\x01]*\x01[^\x01]*$/), line, counter
          match = line.match /^(.*)\x7f(.*)\x01([[:digit:]]*),([[:digit:]]*)$/
          tag = Tag.new
          unless match
            tag.offset = nil
            match = line.match /^(.*)\x7f(.*)\x01([[:digit:]]*)$/
          end
          enforce_line match, line, counter
          #assert filename
          tag.name = match[2]
          tag.file = filename
          tag.line = match[3].to_i
          tag.text = match[1]
          self[filename] << tag
        else
          enforce_line (line =~ /^[^\x7f]*\x7f[^\x7f]*$/), line, counter
          match = line.match /^(.*)\x7f([[:digit:]]*),([[:digit:]]*)$/
          tag = Tag.new
          unless match
            tag.offset = nil
            match = line.match /^(.*)\x7f([[:digit:]]*)$/
          end
          enforce_line match, line, counter
          #assert filename
          tag.file = filename
          tag.line = match[2].to_i
          tag.text = match[1]
          self[filename] << tag
        end
      end
    end
  end

  def edump output
    @files.each do |filename, tags|
      #$logger.debug "dumping tags for '#{filename}'"
      tags = tags.sort { |lhs, rhs| lhs.line <=> rhs.line }
      deleted = tags.size
      tags.uniq! { |t| [ t.name, t.line ] }
      deleted -= tags.size
      #$logger.debug "#{deleted} duplicate tags deleted" unless deleted == 0
      data = StringIO.new
      tags.each do |tag|
        #assert(tag.file == filename)
        if tag.name and tag.text
          data.write "#{tag.text}\x7f#{tag.name}\x01#{tag.line}"
        else
          tn = (tag.name or tag.text)
          #assert tn
          data.write "#{tn}\x7f#{tag.line}"
        end
        if tag.offset
          data.write ",#{tag.offset}"
        end
        data.puts
      end
      output.puts "\x0c"
      output.puts "#{filename},#{data.size}"
      output.puts data.string
    end
  end

  def fix_missing_text
    newfiles = {}
    @files.each do |filename, tags|
      begin
        File.open filename do |file|
          line = nil
          lineno = 0
          tags = tags.sort { |lhs, rhs| lhs.line <=> rhs.line }
          tags.each do |tag|
            next if tag.text
            while lineno < tag.line
              line = file.gets
              lineno += 1
              unless line
                raise "EOF reached while looking up for tag '#{tag.name}' in #{tag.file}:#{tag.line}"
              end
              line.chomp!
            end
            #match line.match /^.*\b#{tag.name}\b/
            #line = match[0] if match
            tag.text = line
          end
        end
        newfiles[filename] = tags
      rescue StandardError
        #$logger.error $!.message
      end
    end
    @files = newfiles
  end

end
