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

  def allow
    p "allow delivered"
    actor.default :fromB

    # just for the hell of it,
    # def fromB here; if called earlier
    # should fail making timing errors
    # more obvious

    class << self
      def fromB
      end
    end

  end

end

b = Class.new do

  Dramatis::Actor.acts_as self

  def initialize anA
    @anA = anA
    @count = 0
    actor.always :count, true
  end

  def startB
    p ">> startB"
    @anA.fromB
    p "<< startB"
  end

  def count
    p ">count< #{@count}"
    @count
  end

  def increment
    p ">increment<"
    @count += 1
  end

  def shouldDeadlock
  end

end

anA = a.new
aB = b.new anA

aB_cast = Dramatis::Actor::Name( aB ).continue nil

c = aB.count

raise "hell #{c}" if c != 0

aB.increment

raise "hell" if aB.count != 1

aB_cast.increment

Dramatis::Runtime.the.quiesce

raise "hell" if aB.count != 2

aB_cast.startB
aB_cast.increment

Dramatis::Runtime.the.quiesce

raise "hell" if aB.count != 2

raise "hell" if Dramatis::Runtime.the.exceptions.length != 0

begin
  aB.shouldDeadlock
  raise "this should raise a deadlock since aB should be waiting on the fromB rpc"
rescue Dramatis::Deadlock
  # note: this clears the pending methods and continuations, so the count should still be
  # 2 and the startB isn't pending anymore
  p "good! got it"
  pp Dramatis::Runtime.the.exceptions
  raise "hell #{Dramatis::Runtime.the.exceptions.length}" if Dramatis::Runtime.the.exceptions.length != 2
  Dramatis::Runtime.the.clear_exceptions
  raise "hell" if Dramatis::Runtime.the.exceptions.length != 0
end

raise "hell" if aB.count != 2

p "b4 allow"
anA.allow
p "a4 allow"

# should get through fine now

aB_cast.startB
aB_cast.increment

Dramatis::Runtime.the.quiesce

aise "hell" if aB.count != 3



