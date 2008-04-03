require File.join( File.dirname(__FILE__), "..", "..", '/spec_helper.rb' )

require 'dramatis/runtime'
require 'dramatis'
require 'dramatis/actor/name'

describe Dramatis::Actor::Name do

  after do
    Dramatis::Runtime.the.quiesce
    warn "after " + Thread.list.join( " " ) if Thread.list.length != 1
    Thread.list.length.should equal( 1 )
  end

  it "should return NoMethodError as appropriate" do
    actor = Dramatis::Actor.new Object.new
    lambda { actor.foo }.should raise_error( NoMethodError )
  end

  it "should recreate errors rather just forward them(?)"

  it "should block other methods during a continuation"

  it "should be creatable unbound" do
    Dramatis::Actor.new
  end

  it "should allow messages to unbound" do
    lambda { Dramatis::Actor.new.foo }.
      should raise_error( Dramatis::Deadlock )
  end

  it "should be creatable bound" do
    name = Dramatis::Actor.new Object.new
    name.should be_kind_of( Dramatis::Actor::Name )
  end

  it "should allow and execute messages to bound names" do
    object = mock Object.new
    object.should_receive(:foo).with(:bar).and_return(:foobar)
    name = Dramatis::Actor.new object
    result = name.foo :bar
    result.should equal( :foobar )
  end

  it "should deliver messages with nil continuations" do
    object = mock(Object.new)
    object.should_receive(:foo).with(:bar)
    name = Dramatis::Actor.new object
    Dramatis::Actor::Name( name ).continue(nil).foo( :bar )
  end

  it "should have a nice short method for casts" do
    object = mock(Object.new)
    object.should_receive(:foo).with(:bar)
    name = Dramatis::Actor.new object
    Dramatis::Actor::cast( name ).foo( :bar )
  end

  it "should suport cast from the object interface"

  it "shouldn't be possible to bind twice" do
    name = Dramatis::Actor.new
    Dramatis::Actor::Name( name ).bind Object.new
    lambda { Dramatis::Actor::Name( name ).bind Object.new }.should raise_error( Dramatis::BindError )
  end

  it "should allow and execute block continuations" do

    actor = Object.new
    name = Dramatis::Actor.new actor
    actor.should_receive(:foo).with(:bar).and_return(:foobar)

    result = nil
    retval = ( Dramatis::Actor::Name( name ).continue  {|value| result = value } ).foo :bar
    retval.should be_nil
    result.should be_nil

    Dramatis::Runtime.the.quiesce
    
    result.should equal( :foobar )

  end

  it "should execute messages to unbound names once bound" do

    name = Dramatis::Actor.new

    object = mock(Object.new)
    object.should_receive(:foo).with(:bar).and_return(:foobar)

    result = nil

    retval = ( Dramatis::Actor::Name( name ).continue { |value| result = value } ).foo :bar

    retval.should be_nil
    result.should be_nil

    Dramatis::Runtime.the.quiesce

    result.should be_nil

    Dramatis::Actor::Name( name ).bind object

    Dramatis::Runtime.the.quiesce

    result.should equal( :foobar )

  end

  it "should be possible to bind with a non-rpc continuation" do
    name = Dramatis::Actor.new
    result = nil
    name = Dramatis::Actor::Name( name ).continue { |v| result = v }
    retval = Dramatis::Actor::Name( name ).bind Object.new
    retval.should equal( nil )
    result.should equal( nil )
    Dramatis::Runtime.the.quiesce
    result.should_not be_nil
  end

  it "should provide a url, if asked" do
    actor = Dramatis::Actor.new Object.new
    url = Dramatis::Actor::Name( actor ).url
    url.should match( %r[http://] )
  end

  it "unbound names should queue messages and deliver them in order"

  it "messages should be delivered out of order sometimes"

  it "flushing should guarantee message order"

end
