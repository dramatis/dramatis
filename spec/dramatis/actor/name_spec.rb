require File.join( File.dirname(__FILE__), "..", "..", '/spec_helper.rb' )

require 'dramatis/runtime'
require 'dramatis/actor/name'

describe Dramatis::Actor::Name do

  Runtime = Dramatis::Runtime
  Actor = Dramatis::Actor
  Name = Actor::Name

  after do
    Runtime.quiesce
  end

  it "should be creatable unbound" do
    name = Name.new
  end

  it "should allow messages to unbound" do
    name = Name.new
    name.foo
  end

  it "should be creatable bound" do
    name = Name.new Object.new
  end

  it "should allow and execute messages to bound names" do
    object = mock(Object.new)
    object.should_receive(:foo).with(:bar).and_return(:foobar)
    name = Name.new object
    result = name.foo :bar
    result.should == :foobar
  end

  it "should deliver messages with nil continuations" do
    object = mock(Object.new)
    object.should_receive(:foo).with(:bar)
    name = Name.new object
    Actor::Name( name ).continue.foo( :bar )
  end

  it "shouldn't be possible to bind twice" do
    name = Name.new
    Actor::Name( name ).bind Object.new
    lambda { Actor::Name( name ).bind Object.new }.should raise_error
  end

  it "should execute messages to unbound names once bound" do
    name = Name.new
    ( Actor::Name( name ).continue { |result| result.should == :foobar } ).foo :bar

    object = mock(Object.new)
    object.should_receive(:foo).with(:bar).and_return(:foobar)

    Actor::Name( name ).bind object
  end

  it "should allow nil continuations and not return anything" do
    object = mock(Object.new)
    object.should_receive(:foo).with(:bar).and_return(:foobar)
    name = Name.new object
    result = ( Actor::Name( name ).continue nil ).foo :bar
    result.should == nil
  end

  it "unbound names should queue messages and deliver them in order"

  it "messages should be delivered out of order sometimes"

  it "flushing should guarantee message order"

end
