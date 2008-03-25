#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor/name'
require 'dramatis/runtime'

require 'pp'

a = Class.new do

  Dramatis::Actor.acts_as self

  def a
    p "b4b"
    begin
      actor.name.b
    rescue Exception => e
      p "got a #{e}"
      raise e
    end
    p "a4b"
  end
  
  def b
    p "bbb"
  end

end

# non-call threaded
# This should deadlock nicely

begin
  p "b4"
  a.new.a
  p "a4"
  raise "should have deadlocked"
rescue Dramatis::Deadlock
  p "got it. yeah!"
end

p "done"
