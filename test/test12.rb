#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor/name'
require 'dramatis/runtime'

require 'pp'

include Dramatis

a = Class.new do

  include Dramatis::Actor

  attr_reader :block_called

  def initialize
    actor.refuse :c
    actor.always :block_called, true
  end

  def a other
    block = lambda do |c|
      # warn "block continuation #{c}"
      @block_called = true
    end
    ( interface( other ).continue( &block ) ).b
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
( interface( a1 ).continue nil ).a a2

Dramatis::Runtime.current.quiesce

raise "hell" if a1.block_called

a2.enable

Dramatis::Runtime.current.quiesce

raise "hell" if !a1.block_called


