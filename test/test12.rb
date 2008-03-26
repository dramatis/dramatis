#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor/name'
require 'dramatis/runtime'

require 'pp'

a = Class.new do

  Dramatis::Actor.acts_as self

  attr_reader :block_called

  def initialize
    actor.refuse :c
    actor.always :block_called, true
  end

  def a other
    block = lambda do |c|
      warn "block continuation #{c}"
      @block_called = true
    end
    ( Dramatis::Actor::Name( other ).continue( &block ) ).b
    other.c
  end
  
  def enable
    actor.default :c
  end

  def b
  end

  def c
  end

end

# non-call threaded

a1 = a.new
a2 = a.new
( Dramatis::Actor::Name( a1 ).continue nil ).a a2

Dramatis::Runtime.the.quiesce

raise "hell" if a1.block_called

a2.enable

Dramatis::Runtime.the.quiesce

raise "hell" if !a1.block_called


