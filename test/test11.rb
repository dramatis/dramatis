#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor/name'
require 'dramatis/actor'
require 'dramatis/runtime'

require 'pp'

# call threading test

a = Class.new do

  Dramatis::Actor.acts_as self

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

  def e
    block = lambda do
      warn "block continuation"
    end
    ( Dramatis::Actor::Name( actor.name ).continue( &block ) ).f
  end

  def f
    warn "f"
  end

end

# recursion allowd

# a.new.a

# co-recursion allowed

# a.new.c

# recusion through blocks ...

a.new.e

Dramatis::Runtime.the.quiesce
