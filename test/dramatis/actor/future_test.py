#!/bin/env python

import inspect
import sys
import os.path
import threading

from logging import warning

sys.path[0:0] = [ os.path.join( os.path.dirname( inspect.getabsfile( inspect.currentframe() ) ), '..', '..', 'lib' ) ]

from inspect import currentframe
from inspect import getframeinfo

from traceback import format_list
from traceback import extract_tb
from traceback import print_exc

from sys import exc_info

import dramatis
import dramatis.error
from dramatis import interface
Actor = dramatis.Actor

class Future_Test:

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


    def test_future_names(self):
        "should allow future names to be created"

        actor = dramatis.Actor( object() )
        future_name = dramatis.future( actor )

    def test_return_a_future_when_called(self):
        "should return a future when used"

        class O (object):
            def foo(self,arg):
                assert arg == "bar"
                return "foobar"

        actor = dramatis.Actor( O() )

        future_name = dramatis.future( actor )

        x = future_name.foo("bar")
            
        assert isinstance(x,dramatis.Future)

    def test_ready(self):
        "should have a ready? interface"
        class aClass ( dramatis.Actor ):
            def __init__( self ):
                self.actor.always("ready",True)
                self._state = None
                self._future = None
            def caller(self,callee):
                self._future = dramatis.future( callee ).callee()
            @property
            def ready(self):
                # warning( "hi " + str(dramatis.interface( self._future ).ready))
                # raise "hell"
                return dramatis.interface( self._future ).ready
            @property
            def value(self):
                return dramatis.interface( self._future ).value

        class bClass ( dramatis.Actor ):
            @property
            def state(self): return self._state
            def __init__(self):
                self.actor.refuse("callee")
            def allow(self):
                self.actor.default("callee")
            def callee(self):
                warning( "callee" )
                return "foobar"

        a = aClass()
        b = bClass()

        a.caller(b)

        assert not a.ready
    
        b.allow()

        dramatis.Runtime.current.quiesce()
    
        assert a.ready

        assert a.value == "foobar"

'''
  it "should evalute to the right value when used" do

    object = mock Object.new
    object.should_receive(:foo).with(:bar).and_return(:foobar)
    actor = Dramatis.Actor.new object

    future_name = future( actor )

    x = future_name.foo :bar

    x.should be_kind_of( Dramatis.Future )
    x.to_sym.should equal( :foobar )

  end

  it "should raise as appropriate" do

    object = Object.new
    actor = Dramatis.Actor.new object

    future_name = future( actor )

    x = future_name.bar :bar

    x.should be_kind_of( Dramatis.Future )

    lambda { x.to_sym }.should raise_error( NoMethodError )

  end

  it "should act like an object ... to the extent possible" do

    object = mock Object.new
    object.should_receive(:foo).with(:bar).and_return(12345)
    actor = Dramatis.Actor.new object

    future_name = future( actor )

    x = future_name.foo :bar
    
    x.should be_kind_of( Dramatis.Future )
    ( x + 0 ).should == 12345
    ( 0 + x ).should == 12345

  end

  it "should have a value interface" do
    object = mock Object.new
    object.should_receive(:foo).with(:bar).and_return(12345)
    actor = Dramatis.Actor.new object

    future_name = future( actor )

    x = future_name.foo :bar
    
    x.should be_kind_of( Dramatis.Future )

    x = interface( x ).value

    x.should be_kind_of( Fixnum )

    x.should == 12345

  end

  it "should evalute to the right value when used with a delay" do

    aClass = Class.new do
      include Dramatis.Actor
      def inititialize
        actor.always :state
        self->_state = None
        self->_future = None
      end
      def caller callee
        self->_future = future( callee ).callee
      end
    end

    bClass = Class.new do
      include Dramatis.Actor
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
'''
