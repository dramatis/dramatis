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
            assert threading.activeCount() == \
                    1 + len(dramatis.runtime.Scheduler.ThreadPool)
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
                return "foobar"

        a = aClass()
        b = bClass()

        a.caller(b)

        assert not a.ready
    
        b.allow()

        dramatis.Runtime.current.quiesce()
    
        assert a.ready

        assert a.value == "foobar"

    def test_future_eval(self):
        "should evalute to the right value when used"

        class O ( object):
            def foo(self,arg):
                assert arg == "bar"
                return "foobar"

        actor = dramatis.Actor(O())

        future_name = dramatis.future( actor )

        x = future_name.foo("bar")

        assert isinstance(x,dramatis.Future)
        assert str(x) == "foobar"

    def test_raise(self):
        "should raise as appropriate"

        actor = dramatis.Actor( object() )

        future_name = dramatis.future( actor )

        x = future_name.bar( "bar" )

        assert isinstance(x,dramatis.Future)

        okay = False
        try:
            str(x)
        except AttributeError: okay = True
        assert okay

    def test_value_if(self):
        "should have a value interface"

        class O (object):
            def foo(self,bar):
                assert bar == "bar"
                return 12345

        actor = dramatis.Actor(O())

        future_name = dramatis.future( actor )

        x = future_name.foo("bar")
    
        assert isinstance(x,dramatis.Future)

        x = dramatis.interface( x ).value

        assert isinstance(x,int)

        assert x == 12345

    def test_act_like_object(self):
        "should act like an object ... to the extent possible"

        class O (object):
            def foo(self,bar):
                assert bar == "bar"
                return 12345

        actor = dramatis.Actor(O())

        future_name = dramatis.future( actor )

        x = future_name.foo("bar")
    
        assert isinstance(x,dramatis.Future)

        # This isn't general; ad hoc implemented
        # Perhaps not considered pythonic?

        assert ( x + 0 ) == 12345
        assert ( 0 + x ) == 12345

