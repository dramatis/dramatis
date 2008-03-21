#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor/name'
require 'dramatis/runtime'

require 'pp'

a = Class.new do

  Dramatis::Actor.acts_as self

  def initialize
    actor.refuse :fromB
  end

end

b = Class.new do

  Dramatis::Actor.acts_as self

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

( Dramatis::Actor::Name( aB ).continue nil ).startB

warn "b4"
Dramatis::Runtime.the.quiesce # :shutdown => false
warn "a5"
