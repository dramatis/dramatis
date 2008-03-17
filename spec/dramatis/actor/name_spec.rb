require File.join( File.dirname(__FILE__), "..", "..", '/spec_helper.rb' )

require 'dramatis/runtime'
require 'dramatis/actor/name'

describe Dramatis::Actor::Name do

  after do
    Dramatis::Runtime.reset
  end

  after do
    Dramatis::Runtime.the.quiesce
    pp "after", Thread.list
    Thread.list.length.should == 1
  end

  it "should be creatable unbound" do
    Dramatis::Actor.new
  end

  it "should allow messages to unbound" do
    lambda { Dramatis::Actor.new.foo }.
      should raise_error Dramatis::Runtime::Scheduler::Deadlock
  end

  it "should be creatable bound" do
    pending
    name = Dramatis::Actor.new Object.new
    name.should be_kind_of Dramatis::Actor::Name
  end

  it "should allow and execute messages to bound names" do
    pending
    object = mock Object.new
    object.should_receive(:foo).with(:bar).and_return(:foobar)
    name = Dramatis::Actor.new object
    result = name.foo :bar
    result.should == :foobar
  end

  it "should deliver messages with nil continuations" do
    pending
    object = mock(Object.new)
    object.should_receive(:foo).with(:bar)
    name = Dramatis::Actor.new object
    Dramatis::Actor::Name( name ).continue(nil).foo( :bar )
  end

  it "shouldn't be possible to bind twice" do
    pending
    name = Dramatis::Actor.new
    pending
    Actor::Name( name ).bind Object.new
    lambda { Actor::Name( name ).bind Object.new }.should raise_error
  end

  it "should execute messages to unbound names once bound" do
    pending
    name = Dramatis::Actor.new

    pending

    ( Dramatis::Actor::Name( name ).continue { |result| result.should == :foobar } ).foo :bar

    object = mock(Object.new)
    object.should_receive(:foo).with(:bar).and_return(:foobar)

    pending

    Dramatis::Actor::Name( name ).bind object
  end

  it "should be possible to bind with a nil continuation" do
    pending
    name = Dramatis::Actor.new
    pending
    name = Dramatis::Actor::Name( name ).continue nil
    Dramatis::Actor::Name( name ).bind Object.new
    pending "should test that the nil continuation is respected"
  end

  it "should allow nil continuations and not return anything" do
    pending
    object = mock(Object.new)
    object.should_receive(:foo).with(:bar).and_return(:foobar)
    name = Dramatis::Actor.new object
    pending
    result = ( Actor::Name( name ).continue nil ).foo :bar
    pending
    result.should == nil
  end

  it "unbound names should queue messages and deliver them in order"

  it "messages should be delivered out of order sometimes"

  it "flushing should guarantee message order"

end
