def level0
  level1
end

def level1
  level2
end

def level2
  raise Exception, "What the Fuck!"
end

def main
  level0
end

begin
  main
rescue Exception
  $stderr.puts $!.inspect
  exit 13
end
