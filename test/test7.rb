#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor/name'
require 'dramatis/runtime'

require 'pp'

include Dramatis

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

( interface( aB ).continue nil ).startB

warn "b4"
# Dramatis::Runtime.current.quiesce
warn "a5"

warn "expect a final deadlock"
