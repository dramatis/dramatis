#!/bin/env python

import inspect
import sys
import os.path

sys.path[0:0] = [ os.path.join( os.path.dirname( inspect.getabsfile( inspect.currentframe() ) ), '..', '..', '..', 'lib' ) ]

from logging import warning
import time
import threading

import dramatis.runtime

sys.path[0:0] = [ os.path.join( os.path.dirname( inspect.getabsfile( inspect.currentframe() ) ), '..' ) ]
from test_helper import DramatisTestHelper

class Become_Test ( DramatisTestHelper ):

    def teardown(self):
        self.runtime_check()

    def test_usable_wo_actor(self):
        "it should be usable apart from being an actor"
        class C ( dramatis.Actor.Behavior ): pass
        c = C()
        assert not isinstance(c, dramatis.Actor.Name )

    def test_constructor_called(self):
        "it should call the constructor"
        count = [0]
        class C ( dramatis.Actor.Behavior ):
            def __init__(self):
                count[0] += 1
        c = C()
        assert not isinstance(c, dramatis.Actor.Name )
        assert count[0] == 1

    def test_return_null_unbound(self):
        "it should return None when unbound"
        class C ( dramatis.Actor.Behavior ):
            def __init__(self):
                self.check()
            def check(self):
                assert self.actor.name == None
        c = C()
        c.check()

    def test_not_return_null_bound(self):
        "it should not return None when bound"
        class C ( dramatis.Actor.Behavior ):
            def __init__(self):
                assert self.actor.name == None
            def check(self):
                assert self.actor.name != None
        c = C()
        a = dramatis.Actor( c )
        assert isinstance( a, dramatis.Actor.Name )
        a.check()

    def test_fail_if_bound(self):
        "it should fail if already bound"

        class C ( dramatis.Actor.Behavior ): pass

        c = C()
        dramatis.Actor( c )
        okay = False
        try:
            dramatis.Actor( c )
        except dramatis.error.Bind:
            okay = True
        assert okay

    def setup_becoming(self):
        class B ( dramatis.Actor ):
            @property
            def foobar(self): return "foo"
            def doit(self, other = None):
                if other == None:
                    other = self
                self.actor.become( other )
        self._B = B
        class C ( dramatis.Actor.Behavior ):
            @property
            def foobar(self): return "bar"
            def check(self, name = None):
                assert self.actor.name == name
            def doit(self, other = None):
                if other == None:
                    other = self
                self.actor.become( other )
        self._C = C
        b = self._b = B()
        assert isinstance( b, dramatis.Actor.Name )
        assert not isinstance( b, B )
        c = self._c = self._C()
        assert not isinstance( c, dramatis.Actor.Name )
        assert isinstance( c, C )

    def test_fail_if_bound(self):
        "it should fail to become a bound behavior"
        self.setup_becoming()
        self._b.doit( self._c )
        self._c.check( self._b )
        self._d = self._B()
        okay = False
        try:
            self._d.doit( self._c )
        except dramatis.error.Bind:
            okay = True
        assert okay

    def test_subclass_rel(self):
        class C ( dramatis.Actor ):
            def check(self):
                assert isinstance(self,C)
                assert isinstance(self,dramatis.Actor)
                assert isinstance(self,dramatis.Actor.Behavior)
        C().check()

    def test_change_behavior(self):
        "it should change behavior on become"
        self.setup_becoming()
        assert self._b.foobar == "foo"
        assert self._c.foobar == "bar"
        self._b.doit( self._c )
        assert self._b.foobar == "bar"
    
    def test_become_oneself(self):
        "it should be okay to become oneself"
        self.setup_becoming()
        self._b.doit()

    def test_connect_disconnect(self):
        "it should connect/disccount actor state accesor on become"
        self.setup_becoming()
        self._c.check( None )
        self._b.doit( self._c )
        self._c.check( self._b )
        self._b.doit( self._C() )
        self._c.check( None )

    def test_concurrency(self):
        "it should expose concurrency via become"
        class B(object):
            def foo(self): pass
        class A( dramatis.Actor ):
            def doit(self):
                me = self.actor.name
                self.actor.become( B() )
                assert self.actor.name == None
                me.foo()
        A().doit()

    def test_sleep(self):
        "it should sleep for the allowed time"
        class C ( dramatis.Actor ):
            def __init__(self):
                self.actor.actor_yield( 0.2 )
        t = time.time()
        c = C()
        assert ( time.time() - t ) < 0.4
        assert ( time.time() - t  ) >= 0.2

    def test_no_gating(self):
        "it should not gate off the caller"

        class C ( dramatis.Actor ):
            def f(self):
                self.actor.actor_yield( 0.2 )
            def g(self): pass
            
        t = time.time()
        c = C()
        assert ( time.time() - t ) < 0.2
        dramatis.release( c ).f()
        assert ( time.time() - t ) < 0.2
        c.g()
        assert ( time.time() - t ) < 0.2
        time.sleep( 0.05 )
        c.g()
        assert ( time.time() - t  ) < 0.2

        def test_notify_become(self):

            "it should call a notification method if present (become)"
            
            count = [0]

            class B ( dramatis.Actor ):
                def __init__(self, other):
                    self.factor.become( other )

            class C ( dramatis.Actor.Behavior ):
                def dramatis_bound(self):
                    count[0] += 1
                    
            assert count[0] == 0
            c = C()
            assert count[0] == 0
            B(c)
            assert count[0] == 1

        def test_notify_new(self):
            "should call a notification method if present (actor new)"

            count = [0]

            class C ( dramatis.Actor.Behavior ):
                def dramatis_bound(self):
                    count[0] += 1

            assert count[0] == 0
            c = C()
            assert count[0] == 0
            dramatis.Actor( c )
            assert count[0] == 1

