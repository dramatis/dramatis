require File.join( File.dirname(__FILE__), "..", '/spec_helper.rb' )

require 'dramatis/runtime'
require 'dramatis/actor/name'

describe Dramatis::Actor do

  after do
    Dramatis::Runtime.the.exceptions.length.should equal 0
    Dramatis::Runtime.the.quiesce
    Thread.list.length.should equal 1
  end

  Actor = Dramatis::Actor

  it "should be creatable from an acts_as class and return the right type" do

    f = Class.new do
      Actor::acts_as self
    end

    name = f.new
    name.should be_a_kind_of( Actor::Name )

  end

  it "should not insert new if not asked to" do

    f = Class.new do
      Actor::acts_as self, :new => :object
    end

    name = f.new
    name.should_not be_a_kind_of( Actor::Name )

  end

  it "should create a new name when invoked with new" do
    name = Actor.new Object.new
    name.should be_a_kind_of Actor::Name
  end

  it "should deadlock if an rpc is made to an unbound name" do
    lambda { Dramatis::Actor.new.foo }.should raise_error Dramatis::Deadlock
  end

  it "should return NoMethodError even when not a direct call" do
    cls = Class.new do
      Dramatis::Actor.acts_as self
      def rpc other
        other.foo
      end
    end
    a = cls.new
    b = cls.new
    lambda { a.rpc b }.should raise_error NoMethodError
  end

  it "should obey refuse" do
    a = Class.new do
      Dramatis::Actor.acts_as self
      def initialize
        actor.refuse :fromB
      end
    end

    b = Class.new do
      Dramatis::Actor.acts_as self
      def initialize anA
        @anA = anA
      end
      def startB
        @anA.fromB
      end
    end

    anA = a.new
    aB = b.new anA

    ( Dramatis::Actor::Name( aB ).continue nil ).startB

    Dramatis::Runtime::the.warnings = false

    lambda { Dramatis::Runtime.the.at_exit }.should raise_error Dramatis::Runtime::Exception

    Dramatis::Runtime::the.warnings = true

    Dramatis::Runtime.reset

  end

  it "should obey refuse and then recover with default" do

    a = Class.new do
      Dramatis::Actor.acts_as self
      def initialize
        actor.refuse :fromB
      end
      def allow
        actor.default :fromB
        class << self
          def fromB; end
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
        @anA.fromB
      end

      def count
        @count
      end

      def increment
        @count += 1
      end
    end

    anA = a.new
    aB = b.new anA

    aB_cast = Dramatis::Actor::Name( aB ).continue nil

    aB.count.should equal 0

    aB.increment

    aB.count.should equal 1

    aB_cast.increment

    Dramatis::Runtime.the.quiesce

    aB.count.should equal 2
    
    aB_cast.startB
    aB_cast.increment

    Dramatis::Runtime.the.quiesce

    aB.count.should equal 2

    anA.allow

    Dramatis::Runtime.the.quiesce

    aB.count.should equal 3
  end

  it "should block calls when in an rpc is inflight and there is no call threading" do

    a = Class.new do

      Dramatis::Actor.acts_as self

      def initialize
        actor.refuse :fromB
      end

      def allow
        actor.default :fromB

        # just for the hell of it,
        # def fromB here; if called earlier
        # should fail making timing errors
        # more obvious

        class << self
          def fromB; end
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
        @anA.fromB
      end

      def count
        @count
      end

      def increment
        @count += 1
      end

      def shouldDeadlock; end

    end

    anA = a.new
    aB = b.new anA

    aB_cast = Dramatis::Actor::Name( aB ).continue nil

    aB.count.should equal 0

    aB.increment

    aB.count.should equal 1

    aB_cast.increment

    Dramatis::Runtime.the.quiesce

    aB.count.should equal 2

    aB_cast.startB
    aB_cast.increment

    Dramatis::Runtime.the.quiesce

    aB.count.should equal 2

    Dramatis::Runtime.the.exceptions.length.should equal 0

    Dramatis::Runtime::the.warnings = false

    lambda { aB.shouldDeadlock }.should raise_error Dramatis::Deadlock

    lambda { Dramatis::Runtime.the.quiesce }.should raise_error Dramatis::Runtime::Exception

    Dramatis::Runtime::the.warnings = true

    Dramatis::Runtime.the.exceptions.length.should equal 2

    Dramatis::Runtime.the.clear_exceptions

    Dramatis::Runtime.the.exceptions.length.should equal 0

    aB.count.should equal 2
    
    anA.allow

    aB_cast.startB
    aB_cast.increment

    Dramatis::Runtime.the.quiesce

    aB.count.should equal 3

  end

  it "should block on recursion in the non-call threaded case" do
    a = Class.new do
      Dramatis::Actor.acts_as self
      def a
        actor.name.b
      end
      def b; end
    end

    lambda { a.new.a }.should raise_error Dramatis::Deadlock
  end

  it "should block block continuations during an rpc w/o call threading " do
    a = Class.new do
      Dramatis::Actor.acts_as self
      attr_reader :block_called
      def initialize
        @block_called = false
        actor.refuse :c
        actor.always :block_called, true
      end
      def a other
        block = lambda { |c| @block_called = true }
        ( Dramatis::Actor::Name( other ).continue( &block ) ).b
        other.c
      end
      def enable
        actor.default :c
      end
      def b; end
      def c; end
    end

    a1 = a.new
    a2 = a.new
    ( Dramatis::Actor::Name( a1 ).continue nil ).a a2

    Dramatis::Runtime.the.quiesce

    a1.block_called.should be_false

    a2.enable

    Dramatis::Runtime.the.quiesce

    a1.block_called.should be_true
  end

  it "should call exception blocks on exceptions" do
    a = Class.new do
      Dramatis::Actor.acts_as self
      attr_reader :block_called, :exception_raised
      def initialize
        actor.refuse :c
        actor.always :block_called, true
        actor.always :exception_raised, true
        @block_called = @exception_raised = false
      end
      def a other
        result = lambda { |r| @block_called = true }
        except = lambda do |exception|

          # FIX: lambda is overridden, I think, so get it back to
          # the spec class and normal processing should work

          # rspec seems to have problems with normal "should" stuff
          # in this block ... this causes a failure, which is good
          # (though it does cascacde, which isn't great, but not worth
          # tracking down).
          raise exception if exception.to_s != "hell"
          @exception_raised = true
        end
        ( Dramatis::Actor::Name( other ).continue :exception => except, &result ).bb
        ( Dramatis::Actor::Name( other ).continue :exception => except, &result ).b
        other.c
      end
      def enable
        actor.default :c
      end
      def bb; end
      def b
        raise "hell"
      end
      def c; end
    end

    a1 = a.new
    a2 = a.new
    ( Dramatis::Actor::Name( a1 ).continue nil ).a a2

    Dramatis::Runtime.the.quiesce

    a1.block_called.should be_false
    a1.exception_raised.should be_true

    a2.enable

    Dramatis::Runtime.the.quiesce

    a1.block_called.should be_true
  end

  it "should allow recursion and corecursion when call threading enabled" do
    a = Class.new do
      Dramatis::Actor.acts_as self
      def initialize
        actor.enable_call_threading
      end
      def a
        actor.name.b
      end
      def b; end
      def c
        other = self.class.new
        other.d actor.name
      end
      def d first
        first.a
      end
    end
    a.new.a # recursion
    a.new.c # co-recursion
  end

end
