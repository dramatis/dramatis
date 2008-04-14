require File.join( File.dirname(__FILE__), "..", "..", '/spec_helper.rb' )

require 'dramatis/actor'
require 'dramatis'

# NB: don't use the module name here: rspec wants to include described
# modules ... and making the result into an actor, well, needless to say,
# it's not a good idea

describe "Dramatis::Actor" do

  include Dramatis

  after do
    begin
      Dramatis::Runtime.the.quiesce
      Dramatis::Runtime.the.exceptions.length.should equal( 0 )
      Thread.list.length.should equal( 1 )
    ensure
      Dramatis::Runtime.reset
    end
  end

  it "should allow future names to be created" do

    object = Object.new
    actor = Dramatis::Actor.new object

    future_name = future( actor )

  end

  it "should return a future when used" do

    object = mock Object.new
    object.should_receive(:foo).with(:bar).and_return(:foobar)
    actor = Dramatis::Actor.new object

    future_name = future( actor )

    x = future_name.foo :bar

    x.should be_kind_of( Dramatis::Runtime::Future )

  end

  it "should evalute to the right value when used" do

    object = mock Object.new
    object.should_receive(:foo).with(:bar).and_return(:foobar)
    actor = Dramatis::Actor.new object

    future_name = future( actor )

    x = future_name.foo :bar

    x.should be_kind_of( Dramatis::Runtime::Future )
    x.to_sym.should equal( :foobar )

  end

  it "should raise as appropriate" do

    object = Object.new
    actor = Dramatis::Actor.new object

    future_name = future( actor )

    x = future_name.bar :bar

    x.should be_kind_of( Dramatis::Runtime::Future )

    lambda { x.to_sym }.should raise_error( NoMethodError )

  end

  it "should act like an object ... to the extent possible" do

    object = mock Object.new
    object.should_receive(:foo).with(:bar).and_return(12345)
    actor = Dramatis::Actor.new object

    future_name = future( actor )

    x = future_name.foo :bar
    
    x.should be_kind_of( Dramatis::Runtime::Future )
    ( x + 0 ).should == 12345
    ( 0 + x ).should == 12345

  end

  it "should have a value interface" do
    object = mock Object.new
    object.should_receive(:foo).with(:bar).and_return(12345)
    actor = Dramatis::Actor.new object

    future_name = future( actor )

    x = future_name.foo :bar
    
    x.should be_kind_of( Dramatis::Runtime::Future )

    x = Future( x ).value

    x.should be_kind_of( Fixnum )

    x.should == 12345

  end

  it "should have a ready? interface" do
    aClass = Class.new do
      include Dramatis::Actor
      def inititialize
        actor.always :ready?
        @state = nil
        @future = nil
      end
      def caller callee
        @future = future( callee ).callee
      end
      def ready?
        Future( @future ).ready?
      end
      def value
        Future( @future ).value
      end
    end

    bClass = Class.new do
      include Dramatis::Actor
      attr_reader :state
      def initialize
        actor.refuse :callee
      end
      def allow
        actor.default :callee
      end
      def callee
        :foobar
      end
    end

    a = aClass.new
    b = bClass.new

    a.caller b

    a.ready?.should be_false
    
    b.allow

    Dramatis::Runtime.the.quiesce
    
    a.ready?.should be_true

    a.value.should equal( :foobar )

  end

  it "should evalute to the right value when used with a delay" do

    aClass = Class.new do
      include Dramatis::Actor
      def inititialize
        actor.always :state
        @state = nil
        @future = nil
      end
      def caller callee
        @future = future( callee ).callee
      end
    end

    bClass = Class.new do
      include Dramatis::Actor
      attr_reader :state
      def initialize
        actor.refuse :callee
      end
      def allow
        actor.default :callee
      end
      def callee
        :foobar
      end
    end

    a = aClass.new
    b = bClass.new

  end

end
