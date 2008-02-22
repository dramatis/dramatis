require File.join( File.dirname(__FILE__), "..", "..", '/spec_helper.rb' )

require 'dramatis/runtime'
require 'dramatis/actor/name'

Runtime = Dramatis::Runtime
Actor = Dramatis::Actor
Name = Actor::Name

describe Name do

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

  it "should execute messages to unbound names once bound" do
    name = Name.new
    ( Actor::Name( name ).continue { |result| result.should == :foobar } ).foo :bar

    object = mock(Object.new)
    object.should_receive(:foo).with(:bar).and_return(:foobar)

    Actor::Name( name ).become object
    pending "can't actually check the should_recieve until queing/quiescing works"
  end

  it "nil continuations should be fine" do
    name = Name.new
    ( Actor::Name( name ).continue ).foo :bar
    Actor::Name( name ).become object
    object = mock(Object.new)
    object.should_receive(:foo).with(:bar).and_return(:foobar)
    pending "can't actually check the should_recieve until queing/quiescing works"

  end

  it "unbound names should queue messages" do
    name = Actor::Name.new
    name.a
    name.b
    Actor
  end

end
