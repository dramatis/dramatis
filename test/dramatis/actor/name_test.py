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

class Name_Test:

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

    def test_attribute_error_no_atts(self):
        "should return AttributeError as appropriate"
        actor = dramatis.Actor( object() )
        okay = False
        try:
            actor.foo()
            raise Exception("should not be reached")
        except AttributeError, ae:
            assert str(ae) == "'object' object has no attribute 'foo'"
            okay = True
        assert okay

    def test_attribute_error(self):
        "should return AttributeError as appropriate"
        o = object()
        actor = dramatis.Actor( o )
        okay = False
        try:
            actor.foo()
            raise Exception("should not be reached")
        except AttributeError, ae:
            assert str(ae) == "'object' object has no attribute 'foo'"
            okay = True
        assert okay

    def test_recreate_errors(self):
        "should recreate errors rather just forward them(?)"

    def test_block_methods_during_cont(self):
        "should block other methods during a continuation"

    def test_unbound(self):
        "should be creatable unbound" 
        dramatis.Actor()

    def test_msg_unbound(self):
        "should allow messages to unbound"
        okay = False
        try:
            dramatis.Actor().foo()
            raise Exception("should not be reached")
        except dramatis.Deadlock: okay = True
        assert okay

    def test_creatable_bound(self):
        "should be creatable bound"
        name = dramatis.Actor( object() )
        assert isinstance(name,dramatis.Actor.Name)


    def test_allow_and_exec_msgs(self):
        "should allow and execute messages to bound names"
        class o ( object ):
            def foo(self,arg):
                assert arg == "bar"
                return "foobar"
        name = dramatis.Actor( o() )
        result = name.foo("bar")
        assert result == "foobar"


    def test_delv_releases(self):
        class O (object):
            def foo(self,arg):
                assert arg == "bar"
        name = dramatis.Actor( O() )
        dramatis.interface( name ).continuation(None).foo("bar")

    def test_short_release(self):
        "should have a nice short method for casts"
        class O (object):
            def foo(self,arg):
                assert arg == "bar"
        name = dramatis.Actor( O() )
        dramatis.release( name ).foo( "bar" )
        
    def test_release_from_interface(self):
        "should suport cast from the object interface"

    def test_no_double_binding(self):
        "shouldn't be possible to bind twice"
        name = dramatis.Actor()
        dramatis.interface( name ).bind( object() )
        okay = False
        try:
            dramatis.interface( name ).bind( object() )
            raise Exception("should not be reached")
        except dramatis.error.Bind: okay = True
        assert okay


    def test_allow_exec_blocks(self):
        "should allow and execute block continuations"
        class O (object):
            def foo(self,arg):
                assert arg == "bar"
                return "foobar"
        actor = O()
        name = dramatis.Actor(actor)

        result = []

        def block(value):
            result[:] = [value]
            
        retval = dramatis.interface( name ).continuation(block).foo( "bar" )
        assert retval == None
        assert result == []
        assert result == []

        dramatis.Runtime.current.quiesce()
    
        assert result == ["foobar"]

    def test_exec_tasks_after_binding(self):
        "should execute messages to unbound names once bound"

        name = dramatis.Actor()

        class O(object):
            def foo(self,arg):
                assert arg == "bar"
                return "foobar"

        result = []

        def block(value):
            result[:] = [ value ]

        retval = dramatis.interface( name ).continuation(block).foo("bar")

        assert retval == None
        assert result == []

        dramatis.Runtime.current.quiesce()

        assert result == []

        dramatis.interface( name ).bind( O() )

        dramatis.Runtime.current.quiesce()
        
        assert result == [ "foobar" ]


    def test_rpc_binds_return_name(self):
        "rpc binds should return an actor name"
        name = dramatis.Actor()
        retval = dramatis.interface( name ).bind( dict() )
        assert isinstance(retval,dramatis.Actor.Name)

    def test_bind_with_release(self):
        "should be possible to bind with a non-rpc continuation"
        name = dramatis.Actor()
        result = []
        def block(v):
            result[:] = [ v ]
        name = dramatis.interface( name ).continuation(block)
        retval = dramatis.interface( name ).bind( object() )
        assert retval == None
        assert result == []
        dramatis.Runtime.current.quiesce()
        assert result != []

    def test_url(self):
        "should provide a url, if asked"

    def test_unboudn_queue_ordered(self):
        "unbound names should queue messages and deliver them in order"

    def test_sometimes_out_of_order(self):
        "messages should be delivered out of order sometimes"

    def test_flush_quarantees_order(self):
        "flushing should guarantee message order"

    def test_can_use_call_sytanx(self):
        class Foo( dramatis.Actor ):
            def __call__( self, arg, foo, bar ):
                assert arg == "foobar"
                assert foo == "foo"
                assert bar == "bar"
                return "okay"
        actor = Foo()
        assert actor("foobar", "foo", bar = "bar" ) == "okay"

    def test_can_use_left_shift_sytanx(self):
        class Foo( dramatis.Actor ):
            def __lshift__( self, arg ):
                assert arg == "foobar"
                return "okay"
        actor = Foo()
        assert actor << "foobar" == "okay"
