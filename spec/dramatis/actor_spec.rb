require File.join( File.dirname(__FILE__), "..", '/spec_helper.rb' )

require 'dramatis/runtime'
require 'dramatis/actor/name'

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

  Actor = Dramatis::Actor

  it "should be creatable from an include-ed class and return the right type" do

    f = Class.new do
      include Dramatis::Actor
    end

    name = f.new
    name.should be_a_kind_of( Actor::Name )

  end

  it "should be possible to not get an actor name" do

    f = Class.new do
      include Dramatis::Actor
      class << self
        remove_method :new
      end
    end

    name = f.new
    name.should be_a_kind_of( f )

  end

  it "should create a new name when invoked with new" do
    name = Actor.new Object.new
    name.should be_a_kind_of( Actor::Name )
  end

  it "should deadlock if an rpc is made to an unbound name" do
    lambda { Dramatis::Actor.new.foo }.should raise_error( Dramatis::Deadlock )
  end

  it "should return NoMethodError even when not a direct call" do
    cls = Class.new do
      include Dramatis::Actor
      def rpc other
        other.foo
      end
    end
    a = cls.new
    b = cls.new
    lambda { a.rpc b }.should raise_error( NoMethodError )
  end

  it "should obey refuse" do
    a = Class.new do
      include Dramatis::Actor
      def initialize
        actor.refuse :fromB
      end
    end

    b = Class.new do
      include Dramatis::Actor
      def initialize anA
        @anA = anA
      end
      def startB
        @anA.fromB
      end
    end

    anA = a.new
    aB = b.new anA

    ( dramatis( aB ).continue nil ).startB

    Dramatis::Runtime::the.warnings = false

    lambda { Dramatis::Runtime.the.at_exit }.should raise_error( Dramatis::Runtime::Exception )

    Dramatis::Runtime::the.warnings = true

    Dramatis::Runtime.reset

  end

  it "should obey refuse and then recover with default" do

    a = Class.new do
      include Dramatis::Actor
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
      include Dramatis::Actor
      include Dramatis::Actor
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

    aB_cast = dramatis( aB ).continue nil

    aB.count.should equal( 0 )

    aB.increment

    aB.count.should equal( 1 )

    aB_cast.increment

    Dramatis::Runtime.the.quiesce

    aB.count.should equal( 2 )
    
    aB_cast.startB
    aB_cast.increment

    Dramatis::Runtime.the.quiesce

    aB.count.should equal( 2 )

    anA.allow

    Dramatis::Runtime.the.quiesce

    aB.count.should equal( 3 )
  end

  it "should block calls when in an rpc is inflight and there is no call threading" do

    a = Class.new do

      include Dramatis::Actor

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

      include Dramatis::Actor

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

    aB_cast = dramatis( aB ).continue nil

    aB.count.should equal( 0 )

    aB.increment

    aB.count.should equal( 1 )

    aB_cast.increment

    Dramatis::Runtime.the.quiesce

    aB.count.should equal( 2 )

    aB_cast.startB
    aB_cast.increment

    Dramatis::Runtime.the.quiesce

    aB.count.should equal( 2 )

    Dramatis::Runtime.the.exceptions.length.should equal( 0 )

    Dramatis::Runtime::the.warnings = false

    lambda { aB.shouldDeadlock }.should raise_error( Dramatis::Deadlock )

    lambda { Dramatis::Runtime.the.quiesce }.should raise_error( Dramatis::Runtime::Exception )

    Dramatis::Runtime::the.warnings = true

    Dramatis::Runtime.the.exceptions.length.should equal( 2 )

    Dramatis::Runtime.the.clear_exceptions

    Dramatis::Runtime.the.exceptions.length.should equal( 0 )

    aB.count.should equal( 2 )
    
    anA.allow

    aB_cast.startB
    aB_cast.increment

    Dramatis::Runtime.the.quiesce

    aB.count.should equal( 3 )

  end

  it "should block on recursion in the non-call threaded case" do
    a = Class.new do
      include Dramatis::Actor
      def a
        actor.name.b
      end
      def b; end
    end

    lambda { a.new.a }.should raise_error( Dramatis::Deadlock )
  end

  it "should block block continuations during an rpc w/o call threading " do
    a = Class.new do
      include Dramatis::Actor
      attr_reader :block_called
      def initialize
        @block_called = false
        actor.refuse :c
        actor.always :block_called, true
      end
      def a other
        block = lambda { |c| @block_called = true }
        ( dramatis( other ).continue( &block ) ).b
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
    ( dramatis( a1 ).continue nil ).a a2

    Dramatis::Runtime.the.quiesce

    a1.block_called.should be_false

    a2.enable

    Dramatis::Runtime.the.quiesce

    a1.block_called.should be_true
  end

  it "should call exception blocks on exceptions" do
    a = Class.new do
      include Dramatis::Actor
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
        ( dramatis( other ).continue :exception => except, &result ).bb
        ( dramatis( other ).continue :exception => except, &result ).b
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
    ( dramatis( a1 ).continue nil ).a a2

    Dramatis::Runtime.the.quiesce

    a1.block_called.should be_false
    a1.exception_raised.should be_true

    a2.enable

    Dramatis::Runtime.the.quiesce

    a1.block_called.should be_true
  end

  it "should allow recursion and corecursion when call threading enabled" do
    a = Class.new do
      include Dramatis::Actor
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
  
  it "should map self returns into an actor name" do
    a = Class.new do
      include Dramatis::Actor
      def me
        self
      end
    end
    anA = a.new
    anA.should be_kind_of( Dramatis::Actor::Name )
    anA.me.should be_kind_of( Dramatis::Actor::Name )
  end

  it "should map self in actor method calls to name" do
    a = Class.new do
      include Dramatis::Actor
      def test
        actor.always :f, true
        actor.name.f self
      end
      def f ref
        ref.object_id != self.object_id
      end
    end
    anA = a.new
    anA.test.should be_true
  end

  it "should raise deadlocks with pretty backtraces" do

    a = Class.new do
      include Dramatis::Actor
      def deadlock
        @@first_line = __LINE__.to_i + 1
        actor.name.deadlock
      end
      def self.first_line
        @@first_line
      end
    end

    anA = a.new

    second_line = nil

    begin 
      second_line = __LINE__.to_i + 1
      anA.deadlock
      raise "fail: should not get here"
    rescue Dramatis::Deadlock => deadlock
      bt = deadlock.backtrace

      f, l = bt[0].split ':'
      f.should == __FILE__
      l.to_i.should == a.first_line

      f, l = bt[1].split ':'
      f.should == __FILE__
      l.to_i.should == second_line

    end
    
  end

end
