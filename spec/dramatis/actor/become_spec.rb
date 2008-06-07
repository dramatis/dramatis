require File.join( File.dirname(__FILE__), "..", "..", '/spec_helper.rb' )

require 'dramatis/actor/behavior'

describe "Dramatis::Actor::Behavior" do

  include DramatisSpecHelper

  after(:each) { runtime_check }

  it "should be usable apart from being an actor" do
    class C
      include Dramatis::Actor::Behavior
    end
    c = C.new
    c.should_not be_a_kind_of( Dramatis::Actor::Name )
  end

  it "should call the constructor" do
    class C
      include Dramatis::Actor::Behavior
      @@count = 0
      def self.count; @@count; end
      def initialize
        @@count += 1
      end
    end
    c = C.new
    c.should_not be_a_kind_of( Dramatis::Actor::Name )
    C.count.should == 1
  end

  it "should return nil when unbound" do
    class C
      include Dramatis::Actor::Behavior
      def initialize
        check
      end
      def check
        actor.name.should == nil
      end
    end
    c = C.new
    c.check
  end

  it "should not return nil when bound" do
    class C
      include Dramatis::Actor::Behavior
      def initialize
        actor.name.should == nil
      end
      def check
        actor.name.should_not == nil
      end
    end
    c = C.new
    a = Dramatis::Actor.new c
    a.should be_a_kind_of( Dramatis::Actor::Name )
    a.check
  end

  it "should fail if already bound" do
    class C
      include Dramatis::Actor::Behavior
    end
    c = C.new
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
      class C
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
      @c = C.new
      @c.should_not be_a_kind_of( Dramatis::Actor::Name )
      @c.should be_a_kind_of( C )
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
      @b.doit C.new
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
      def foo; end
    end

    A.new.doit

  end

end
