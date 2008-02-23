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

  it "should deliver messages with nil continuations" do
    object = mock(Object.new)
    object.should_receive(:foo).with(:bar)
    name = Name.new object
    Actor::Name( name ).continue.foo( :bar )
    pending
  end

  it "should allow and execute messages to bound names" do
    object = mock(Object.new)
    object.should_receive(:foo).with(:bar).and_return(:foobar)
    name = Name.new object
    result = name.foo :bar
    pending
    result.should == :foobar
  end

  it "should execute messages to unbound names once bound" do
    name = Name.new
    ( Actor::Name( name ).continue { |result| result.should == :foobar } ).foo :bar

    object = mock(Object.new)
    object.should_receive(:foo).with(:bar).and_return(:foobar)

    Actor::Name( name ).actor = object

    pending
  end

  it "unbound names should queue messages" do
    name = Actor::Name.new
    name.a
    name.b
    Actor
  end

end
