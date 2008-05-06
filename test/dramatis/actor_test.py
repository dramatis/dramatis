#!/bin/env python

import inspect
import sys
import os.path
import threading

from logging import warning

sys.path[0:0] = [ os.path.join( os.path.dirname( inspect.getabsfile( inspect.currentframe() ) ), '..', '..', 'lib' ) ]

import dramatis
import dramatis.error
from dramatis import interface
Actor = dramatis.Actor

class Actor_Test:

    def setup(self): pass

    def teardown(self):
        try:
            dramatis.Runtime.current.quiesce()
            assert len( dramatis.Runtime.current.exceptions() ) == 0
            assert threading.activeCount() == 1
        finally:
            dramatis.Runtime.reset()

    def test(self):
        assert True

    # it should be creatable as a derived type and return the right type

    def test_included(self):
        class Foo( dramatis.Actor ):
            def __init__(self, *args):
                # warning("! " + str(self))
                # warning("!! " + str(args))
                # logging.warning(type(self))
                super(Foo,self).__init__()
                assert len( args ) == 1
                assert args[0] == "foobar"
            def foo(self):
                return "bar"
        name = Foo( "foobar" )
        # logging.warning( type( name ) )
        assert isinstance( name, dramatis.Actor.Name )
        assert name.foo() == "bar"

    # it should be creatable naked

    def test_naked(self):
        class Foo( object ):
            def __init__(self, *args):
                super(Foo,self).__init__()
                assert len( args ) == 1
                assert args[0] == "foobar"
            def foo(self):
                return "bar"
        name = dramatis.Actor( Foo( "foobar" ) )
        # logging.warning( type( name ) )
        assert isinstance( name, dramatis.Actor.Name )
        assert name.foo() == "bar"

    def test_no_actor_name(self):
        class Foo( dramatis.Actor ):
            class __metaclass__( dramatis.Actor.__metaclass__ ):
                def __call__(cls,*args,**kwds):
                    return type(object).__call__(cls,*args,**kwds)
        name = Foo()
        assert isinstance( name, Foo )

    def test_no_actor_name_simple(self):
        class Foo( dramatis.Actor.Methods ): pass
        name = Foo()
        assert isinstance( name, Foo )

    def test_naked_again(self):
        "should create a new name when invoked with new"
        name = Actor( object() )
        assert isinstance( name, dramatis.Actor.Name )

    def test_rpc_unbound(self):
        "should deadlock if an rpc is made to an unbound name"
        try:
            Actor().foo()
            raise Exception("should not be reached")
        except dramatis.Deadlock: pass
        # warning('here?')


    def test_no_method(self):
        "should return NoMethodError even when not a direct call"
        class cls(dramatis.Actor):
            def rpc( self, other ):
                other.foo()

        a = cls()
        b = cls()

        try:
            a.rpc(b)
            raise "should not be reached"
        except AttributeError, ae: pass
        
    def test_refuse(self):
        "should obey refuse"
        
        class a ( dramatis.Actor ):
            def __init__(self):
                # warning("REFUSE")
                self.actor.refuse( "fromB" )
                
        class b ( dramatis.Actor ):
            def __init__( self, anA ):
                self._anA = anA

            def startB( self ):
                self._anA.fromB()

        anA = a()
        aB = b( anA )

        ( interface( aB ).continuation( None ) ).startB()

        dramatis.Runtime.current.warnings = False

        try:
            # warning("before at_exti")
            dramatis.Runtime.current.at_exit()
            # warning("after at_exti")
            raise "should not be reached"
        except dramatis.error.Uncaught, u: pass

        dramatis.Runtime.current.warnings = True

        dramatis.Runtime.reset()

    '''
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
        self._anA = anA
        self._count = 0
        actor.always :count, True
      end

      def startB
        self._anA.fromB
      end

      def count
        self._count
      end

      def increment
        self._count += 1
      end
    end

    anA = a.new
    aB = b.new anA

    aB_cast = interface( aB ).continue nil

    aB.count.should equal( 0 )

    aB.increment

    aB.count.should equal( 1 )

    aB_cast.increment

    Dramatis::Runtime.current.quiesce

    aB.count.should equal( 2 )
    
    aB_cast.startB
    aB_cast.increment

    Dramatis::Runtime.current.quiesce

    aB.count.should equal( 2 )

    anA.allow

    Dramatis::Runtime.current.quiesce

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
        self._anA = anA
        self._count = 0
        actor.always :count, True
      end

      def startB
        self._anA.fromB
      end

      def count
        self._count
      end

      def increment
        self._count += 1
      end

      def shouldDeadlock; end

    end

    anA = a.new
    aB = b.new anA

    aB_cast = interface( aB ).continue nil

    aB.count.should equal( 0 )

    aB.increment

    aB.count.should equal( 1 )

    aB_cast.increment

    Dramatis::Runtime.current.quiesce

    aB.count.should equal( 2 )

    aB_cast.startB
    aB_cast.increment

    Dramatis::Runtime.current.quiesce

    aB.count.should equal( 2 )

    Dramatis::Runtime.current.exceptions.length.should equal( 0 )

    Dramatis::Runtime.current.warnings = False

    lambda { aB.shouldDeadlock }.should raise_error( Dramatis::Deadlock )

    lambda { Dramatis::Runtime.current.quiesce }.should raise_error( Dramatis::Error::Uncaught )

    Dramatis::Runtime.current.warnings = True

    Dramatis::Runtime.current.exceptions.length.should equal( 2 )

    Dramatis::Runtime.current.clear_exceptions

    Dramatis::Runtime.current.exceptions.length.should equal( 0 )

    aB.count.should equal( 2 )
    
    anA.allow

    aB_cast.startB
    aB_cast.increment

    Dramatis::Runtime.current.quiesce

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
        self._block_called = False
        actor.refuse :c
        actor.always :block_called, True
      end
      def a other
        block = lambda { |c| self._block_called = True }
        ( interface( other ).continue( &block ) ).b
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
    ( interface( a1 ).continue nil ).a a2

    Dramatis::Runtime.current.quiesce

    a1.block_called.should be_False

    a2.enable

    Dramatis::Runtime.current.quiesce

    a1.block_called.should be_True
  end

  it "should call exception blocks on exceptions" do
    a = Class.new do
      include Dramatis::Actor
      attr_reader :block_called, :exception_raised
      def initialize
        actor.refuse :c
        actor.always :block_called, True
        actor.always :exception_raised, True
        self._block_called = self._exception_raised = False
      end
      def a other
        result = lambda { |r| self._block_called = True }
        except = lambda do |exception|

          # FIX: lambda is overridden, I think, so get it back to
          # the spec class and normal processing should work

          # rspec seems to have problems with normal "should" stuff
          # in this block ... this causes a failure, which is good
          # (though it does cascacde, which isn't great, but not worth
          # tracking down).
          raise exception if exception.to_s != "hell"
          self._exception_raised = True
        end
        ( interface( other ).continue :exception => except, &result ).bb
        ( interface( other ).continue :exception => except, &result ).b
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
    ( interface( a1 ).continue nil ).a a2

    Dramatis::Runtime.current.quiesce

    a1.block_called.should be_False
    a1.exception_raised.should be_True

    a2.enable

    Dramatis::Runtime.current.quiesce

    a1.block_called.should be_True
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
        actor.always :f, True
        actor.name.f self
      end
      def f ref
        ref.object_id != self.object_id
      end
    end
    anA = a.new
    anA.test.should be_True
  end

  it "should raise deadlocks with pretty backtraces" do

    a = Class.new do
      include Dramatis::Actor
      def deadlock
        self._self._first_line = __LINE__.to_i + 1
        actor.name.deadlock
      end
      def self.first_line
        self._self._first_line
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

      # pp bt

      f, l = bt[0].split ':'
      f.should == __FILE__
      l.to_i.should == a.first_line

      f, l = bt[1].split ':'
      f.should == __FILE__
      l.to_i.should == second_line

    end
    
  end


'''
