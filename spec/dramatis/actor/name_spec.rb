require File.join( File.dirname(__FILE__), "..", "..", '/spec_helper.rb' )

require 'dramatis/runtime'
require 'dramatis'
require 'dramatis/actor/name'

describe Dramatis do

  include Dramatis

  include DramatisSpecHelper

  after(:each) { runtime_check }

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
    (Dramatis::Actor::Name === name).should be_true
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
    interface( name ).continue(nil).foo( :bar )
  end

  it "should have a nice short method for casts" do
    object = mock(Object.new)
    object.should_receive(:foo).with(:bar)
    name = Dramatis::Actor.new object
    release( name ).foo( :bar )
  end

  it "should suport cast from the object interface"

  it "shouldn't be possible to bind twice" do
    name = Dramatis::Actor.new
    interface( name ).bind Object.new
    lambda { interface( name ).bind Object.new }.should raise_error( Dramatis::Error::Bind )
  end

  it "should allow and execute block continuations" do

    actor = Object.new
    name = Dramatis::Actor.new actor
    actor.should_receive(:foo).with(:bar).and_return(:foobar)

    result = nil
    retval = ( interface( name ).continue { |value| result = value } ).foo :bar
    retval.should be_nil
    result.should be_nil
    result.should be_nil # to perhaps highlight a threading problem

    Dramatis::Runtime.current.quiesce
    
    result.should equal( :foobar )

  end

  it "should execute messages to unbound names once bound" do

    name = Dramatis::Actor.new

    object = mock(Object.new)
    object.should_receive(:foo).with(:bar).and_return(:foobar)

    result = nil

    retval = ( interface( name ).continue { |value| result = value } ).foo :bar

    retval.should be_nil
    result.should be_nil

    Dramatis::Runtime.current.quiesce

    result.should be_nil

    interface( name ).bind object

    Dramatis::Runtime.current.quiesce

    result.should equal( :foobar )

  end

  it "rpc binds should return an actor name" do
    name = Dramatis::Actor.new
    retval = Dramatis.interface( name ).bind Hash.new
    (Dramatis::Actor::Name === retval).should be_true
  end

  it "should be possible to bind with a non-rpc continuation" do
    name = Dramatis::Actor.new
    result = nil
    name = interface( name ).continue { |v| result = v }
    retval = interface( name ).bind Object.new
    retval.should equal( nil )
    result.should equal( nil )
    Dramatis::Runtime.current.quiesce
    ( nil.class === result ).should_not be_true
  end

  it "should provide a url, if asked" do
    actor = Dramatis::Actor.new Object.new
    url = interface( actor ).url
    url.should match( %r[http://] )
  end

  it "should hash as the actor, not the behavior" do
    behavior = Object.new
    actor = Dramatis::Actor.new behavior
    actor.hash.should_not == behavior.hash
    actor.hash.should_not equal(behavior.hash)
    (actor.hash == behavior.hash).should be_false
  end

  it "should not forward hash requests to the behavior" do
    c = Class.new do
      include Dramatis::Actor
      def initialize
        Hash.new[actor.name] = :foo
      end
    end
    c.new
  end

  it "should hash two actors to different values" do
    b1 = Object.new
    a1 = Dramatis::Actor.new b1
    b2 = Object.new
    a2 = Dramatis::Actor.new b2
    a1.hash.should_not == a2.hash
    (a1 == a2).should be_false
  end

  it "should hash two names to different values, even w/diff cont sematnics" do
    b1 = Object.new
    a1 = Dramatis::Actor.new b1
    a2 = release( a1 )
    a1.hash.should == a2.hash
    (a1 == a2).should be_true
  end

  it "should hash to the actor mailbox/state" do
    # I'm not quite sure if this test should be implemented;
    # The actor state is pretty hidden
    pending
  end

  it "unbound names should queue messages and deliver them in order"

  it "messages should be delivered out of order sometimes"

  it "flushing should guarantee message order"

end
