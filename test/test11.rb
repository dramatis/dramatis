#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor/name'
require 'dramatis/runtime'

require 'pp'

# call threading test

a = Class.new do

  Dramatis::Actor.acts_as self

  def a
    actor.name.b
  end
  
  def b
  end

end

# This should deadlock nicely

begin
  a.new.a
  raise "should have deadlocked"
rescue Dramatis::Deadlock
end
