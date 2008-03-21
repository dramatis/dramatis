#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor/name'
require 'dramatis/runtime'

require 'pp'

cls = Class.new do

  Dramatis::Actor.acts_as self

  def initialize other = nil
    other.f actor.name if other
  end

  def f caller
    warn "f" + caller.g
  end

  def g
    "g"
  end

end

a = cls.new
begin
  b = cls.new a
  raise "should have deadlocked"
rescue Dramatis::Deadlock
  # okay
end
