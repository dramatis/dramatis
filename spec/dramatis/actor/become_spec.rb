require File.join( File.dirname(__FILE__), "..", "..", '/spec_helper.rb' )

require 'dramatis/actor/behavior'

describe "Dramatis::Actor::Behavior" do

  include DramatisSpecHelper

  after(:each) { runtime_check }

  it "should be usable apart from being an actor" do
    _C = Class.new do
      include Dramatis::Actor::Behavior
    end
    c = _C.new
    c.should_not be_a_kind_of( Dramatis::Actor::Name )
  end

  it "should call the constructor" do
    _C = Class.new do
      include Dramatis::Actor::Behavior
      @@count = 0
      def self.count; @@count; end
      def initialize
        @@count += 1
      end
    end
    c = _C.new
    c.should_not be_a_kind_of( Dramatis::Actor::Name )
    _C.count.should == 1
  end

  it "should return nil when unbound" do
    _C = Class.new do
      include Dramatis::Actor::Behavior
      def initialize
        check
      end
      def check
        actor.name.should == nil
      end
    end
    c = _C.new
    c.check
  end

  it "should not return nil when bound" do
    _C = Class.new do
      include Dramatis::Actor::Behavior
      def initialize
        actor.name.should == nil
      end
      def check
        actor.name.should_not == nil
      end
    end
    c = _C.new
    a = Dramatis::Actor.new c
    a.should be_a_kind_of( Dramatis::Actor::Name )
    a.check
  end

  it "should fail if already bound" do
    _C = Class.new do
      include Dramatis::Actor::Behavior
    end
    c = _C.new
    Dramatis::Actor.new c
    lambda { Dramatis::Actor.new c }.should raise_error( Dramatis::Error::Bind )
  end

  describe "becoming" do

    before(:each) do
      class B
        include Dramatis::Actor
        def foobar; "foo"; end
        def doit other = nil
          other = self if other == nil
          actor.become other
        end
      end
      @_C = Class.new do
        include Dramatis::Actor::Behavior
        def foobar; "bar"; end
        def check name = nil
          actor.name.should == name
        end
        def doit other = nil
          other = self if other == nil
          actor.become other
        end
      end
      @b = B.new
      @b.should be_a_kind_of( Dramatis::Actor::Name )
      @b.should_not be_a_kind_of( B )
      @c = @_C.new
      @c.should_not be_a_kind_of( Dramatis::Actor::Name )
      @c.should be_a_kind_of( @_C )
    end

    it "should change behavior on become" do
      @b.foobar.should == "foo"
      @c.foobar.should == "bar"
      @b.doit @c
      @b.foobar.should == "bar"
    end
    
    it "should fail to become a bound behavior" do
      @b.doit @c
      @d = B.new
      lambda { @d.doit @c }.should raise_error
    end

    it "should be okay to become oneself" do
      @b.doit
    end

    it "should connect/disccount actor state accesor on become" do
      @c.check nil
      @b.doit @c
      @c.check @b
      @b.doit @_C.new
      @c.check nil
    end

  end

  it "should expose concurrency via become" do
    class B
      def foo; end
    end
    class A
      include Dramatis::Actor
      def doit
        me = actor.name
        actor.become B.new
        actor.name.should == nil
        me.foo
      end
    end

    A.new.doit

  end

  it "should sleep for the allowed time" do
    _C = Class.new do
      include Dramatis::Actor
      def initialize
        actor.yield 0.2
      end
    end
    t = Time::now
    c = _C.new
    ( Time::now - t ).should < 0.4
    ( Time::now - t  ).should >= 0.2
  end

  it "should not gate off the caller" do
    _C = Class.new do
      include Dramatis::Actor
      def f
        actor.yield 0.2
      end
      def g; end
    end
    t = Time::now
    c = _C.new
    ( Time::now - t ).should < 0.2
    Dramatis.release( c ).f
    ( Time::now - t ).should < 0.2
    c.g
    ( Time::now - t ).should < 0.2
    sleep 0.05
    c.g
    ( Time::now - t  ).should < 0.2
  end

  it "should defer intialization until bound" do
    pending "I'm not sure if this is a good idea"
    _C = Class.new do
      @@a = 1
      def self.a; return @@a; end
      include Dramatis::Actor::Behavior
      def initialize
        @@a += 1
        actor.accept :f
      end
    end
    _C.a.should == 1
    c = _C.new
    _C.a.should == 1
    Dramatis::Actor.new c
    _C.a.should == 2
  end

  it "should call a notification method if present (become)" do
    _B = Class.new do
      include Dramatis::Actor
      def initialize other
        actor.become other
      end
    end
    _C = Class.new do
      @@a = 1
      def self.a; return @@a; end
      include Dramatis::Actor::Behavior
      def dramatis_bound
        @@a += 1
      end
    end
    _C.a.should == 1
    c = _C.new
    _C.a.should == 1
    _B.new c
    _C.a.should == 2
  end

  it "should call a notification method if present (actor new)" do
    _C = Class.new do
      @@a = 1
      def self.a; return @@a; end
      include Dramatis::Actor::Behavior
      def dramatis_bound
        @@a += 1
      end
    end
    _C.a.should == 1
    c = _C.new
    _C.a.should == 1
    Dramatis::Actor.new c
    _C.a.should == 2
  end

end
