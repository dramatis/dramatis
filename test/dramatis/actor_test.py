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
            raise Exception("should not be reached")
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
            raise Exception("should not be reached")
        except dramatis.error.Uncaught, u: pass

        dramatis.Runtime.current.warnings = True

        dramatis.Runtime.reset()

    def test_refuse_default(self):
        "should obey refuse and then recover with default"

        class a ( dramatis.Actor ):
            def __init__(self):
                self.actor.refuse( "fromB" )

            def _fromB(self): pass

            def allow(self):
                self.actor.default( "fromB" )

                self.fromB = self._fromB

        class b ( dramatis.Actor ):
            def __init__( self, anA ):
                self._anA = anA
                self._count = 0
                self.actor.always( "count", True )

            def startB( self ):
                self._anA.fromB()
                
            @property
            def count( self ):
                # warning( "returning " + str( self._count ) )
                return self._count

            def increment( self ):
                self._count += 1
                return self._count

        anA = a()
        aB = b( anA )

        aB_cast = interface( aB ).continuation( None )

        assert aB.count == 0

        aB.increment()

        assert aB.count == 1

        aB_cast.increment()

        dramatis.Runtime.current.quiesce()

        assert aB.count == 2
    
        # warning( "a" )
        aB_cast.startB()
        # warning( "b" )
        aB_cast.increment()
        # warning( "c" )

        dramatis.Runtime.current.quiesce()

        # warning( "here " + str(aB.count ) )

        assert aB.count == 2

        anA.allow()

        dramatis.Runtime.current.quiesce()

        assert aB.count == 3

    def test_block_if_non_threading(self):
        '''should block calls when in an rpc is inflight and
        there is no call threading'''

        class a ( dramatis.Actor ):
            def __init__( self ):
                self.actor.refuse( "fromB" )

            def _fromB( self ): pass

            def allow(self):
                self.actor.default( "fromB" )
                self.fromB = self._fromB

        class b ( dramatis.Actor ):
            def __init__(self, anA):
                self._anA = anA
                self._count = 0
                self.actor.always( "count", True )

            def startB( self ):
                self._anA.fromB()

            @property
            def count( self ):
                return self._count

            def increment(self):
                self._count += 1
                
            def shouldDeadlock(self): pass

        anA = a()
        aB = b(anA)

        aB_cast = interface( aB ).continuation( None )

        assert aB.count == 0

        aB.increment()

        assert aB.count == 1

        aB_cast.increment()

        dramatis.Runtime.current.quiesce()

        assert aB.count == 2

        aB_cast.startB()
        aB_cast.increment()
            
        dramatis.Runtime.current.quiesce()

        assert aB.count == 2

        assert len( dramatis.Runtime.current.exceptions() ) == 0

        dramatis.Runtime.current.warnings = False
            
        try:
            aB.shouldDeadlock()
            raise Exception("should not be reached")
        except dramatis.Deadlock: pass

        try:
            dramatis.Runtime.current.quiesce()
            raise Exception("should not be reached")
        except dramatis.error.Uncaught: pass

        dramatis.Runtime.current.warnings = True

        assert len( dramatis.Runtime.current.exceptions() ) == 2

        dramatis.Runtime.current.clear_exceptions()

        assert len( dramatis.Runtime.current.exceptions() ) == 0

        assert aB.count == 2
    
        anA.allow()

        aB_cast.startB()
        aB_cast.increment()
        
        dramatis.Runtime.current.quiesce()

        assert aB.count == 3


    def test_block_rec_non_thread(self):
        "should block on recursion in the non-call threaded case"
        class a ( dramatis.Actor ):
            def a(self):
                return self.actor.name.b()
            def b(self): pass

        try:
            a().a()
            raise Exception("should have raised deadlock")
        except dramatis.Deadlock: pass

    def test_block_block_conts_rpc_no_threading( self ):
        "should block block continuations during an rpc w/o call threading"
        class a ( dramatis.Actor ):
            @property
            def block_called(self): return self._block_called

            def __init__(self):
                self._block_called = False
                self.actor.refuse("c")
                self.actor.always( "block_called", True )

            def a(self, other):

                def block ( c ):
                    self._block_called = True

                ( interface( other ).continuation( block ) ).b()

                other.c()

            def enable(self):
                self.actor.default("c")

            def b(self): pass
            def c(self): pass

        a1 = a()
        a2 = a()

        ( interface( a1 ).continuation( None ) ).a( a2 )

        dramatis.Runtime.current.quiesce()

        assert not a1.block_called

        a2.enable()

        dramatis.Runtime.current.quiesce()

        assert a1.block_called

    def test_call_except_blocks(self):
        "should call exception blocks on exceptions"
        class a ( dramatis.Actor ):
            @property
            def block_called(self): return self._block_called

            @property
            def exception_raised(self): return self._exception_raised

            def __init__(self):
                self.actor.refuse("c")
                self.actor.always( "block_called", True )
                self.actor.always( "exception_raised", True )
                self._block_called = self._exception_raised = False

            def a(self, other):
                def result(r): self._block_called = True
                def exception(e):
                    if str(e) != "hell": raise e
                    self._exception_raised = True

                ( interface( other ).continuation( { "exception": exception,
                                                     "result": result } ) ).bb()
                ( interface( other ).continuation( { "exception": exception,
                                                     "result": result } ) ).b()

                other.c()

            def enable(self):
                self.actor.default("c")

            def bb(self): pass

            def b(self): raise Exception("hell")

            def c(self): pass

        a1 = a()
        a2 = a()

        ( interface( a1 ).continuation( None ) ).a( a2 )

        dramatis.Runtime.current.quiesce()

        assert not a1.block_called
        assert a1.exception_raised

        a2.enable()

        dramatis.Runtime.current.quiesce()

        assert a1.block_called

    def test_allow_rec_corec_with_threading(self):
        "should allow recursion and corecursion when call threading enabled"
        class a  ( dramatis.Actor ):
            def __init__(self):
                self.actor.enable_call_threading()
            def a(self):
                self.actor.name.b()
            def b(self): pass
            def c(self):
                other = self.__class__()
                other.d( self.actor.name )
            def d(self,first):
                first.a()
        a().a() # recursion
        a().c() # co-recursion
 
    '''
  it "should map self returns into an actor name" do
    a = Class.new do
      include Dramatis.Actor
      def me
        self
      end
    end
    anA = a.new
    anA.should be_kind_of( Dramatis.Actor.Name )
    anA.me.should be_kind_of( Dramatis.Actor.Name )
  end

  it "should map self in actor method calls to name" do
    a = Class.new do
      include Dramatis.Actor
      def test
        actor.always :f, True
        self.actor.name.f self
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
      include Dramatis.Actor
      def deadlock
        self._self._first_line = __LINE__.to_i + 1
        self.actor.name.deadlock
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
      raise Exception("fail: should not get here")
    rescue Dramatis.Deadlock => deadlock
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
