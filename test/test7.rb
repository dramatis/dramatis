#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor/name'
require 'dramatis/runtime'

require 'pp'

a = Class.new do

  include Dramatis::Actor

  def initialize
    actor.refuse :fromB
  end

end

b = Class.new do

  include Dramatis::Actor

  def initialize anA
    @anA = anA
  end

  def startB
    @anA.fromB
  end

  def shouldDeadlock
  end

end

anA = a.new
aB = b.new anA

( dramatis( aB ).continue nil ).startB

warn "b4"
# Dramatis::Runtime.the.quiesce
warn "a5"

warn "expect a final deadlock"
