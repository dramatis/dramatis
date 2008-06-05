#!/usr/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor'

$serial = false

if ARGV[0] == "serial"
  $serial = true
end

class Foo
  if !$serial
    include Dramatis::Actor
  end
  def foo that
    return that.bar -3
  end
end

class Bar
  if !$serial
    include Dramatis::Actor
  end
  def bar value
    fooobar
  end
  def foobar
    "foobar"
  end
end

Foo.new
begin
  Foo.new.foo Bar.new
rescue NameError => ne
  puts "hey, I got a #{ne}"
  puts "it happened here: " + ne.backtrace.join("\n")
end
