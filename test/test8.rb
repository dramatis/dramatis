#!/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor/name'
require 'dramatis/runtime'

require 'pp'

# no call threading case

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
    @count = 0
    actor.always :count
  end

  def startB
    @anA.fromB
  end

  def startB
    @anA.fromB
  end

  def count
    @count
  end

  def increment
    @count += 1
  end

  def shouldDeadlock
  end

end

anA = a.new
aB = b.new anA

aB_cast = Dramatis::Actor::Name( aB ).continue nil

raise "hell" if aB.count != 0

aB.increment

raise "hell" if aB.count != 1

aB_cast.increment

Dramatis::Runtime.the.quiesce

raise "hell" if aB.count != 2

aB_cast.startB
aB_cast.increment

Dramatis::Runtime.the.quiesce

raise "hell" if aB.count != 2

begin
  aB.shouldDeadlock
  raise "this should raise a deadlock since aB should be waiting on the fromB rpc"
rescue Dramatis::Deadlock
end


Dramatis::Runtime.the.quiesce
