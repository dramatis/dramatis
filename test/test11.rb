#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor/name'
require 'dramatis/actor'
require 'dramatis/runtime'

require 'pp'

# call threading test

a = Class.new do

  include Dramatis::Actor

  def initialize
    actor.enable_call_threading
  end

  def a
    actor.name.b
  end
  
  def b
  end

  def c
    other = self.class.new
    other.d actor.name
  end

  def d first
    first.a
  end

end

# recursion allowd

a.new.a

# co-recursion allowed

a.new.c
