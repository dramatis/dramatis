#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor/name'
require 'dramatis/runtime'

require 'pp'

include Dramatis

a = Class.new do

  include Dramatis::Actor
  
  attr_reader :block_called, :exception_raised

  def initialize
    actor.refuse :c
    actor.always :block_called, true
    actor.always :exception_raised, true
  end

  def a other
    result = lambda do |r|
      # warn "block continuation #{r}"
      @block_called = true
    end
    except = lambda do |exception|
      # warn "#{exception} (a good thing)"
      raise "hell: #{exception.to_s}" if exception.to_s != "hell"
      # warn "exception continuation"
      @exception_raised = true
    end
    ( interface( other ).continue :exception => except, &result ).bb
    ( interface( other ).continue :exception => except, &result ).b
    other.c
  end
  
  def enable
    actor.default :c
  end

  def bb
  end

  def b
    raise "hell"
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
raise "hell" if !a1.exception_raised

a2.enable

Dramatis::Runtime.current.quiesce

raise "hell" if !a1.block_called
