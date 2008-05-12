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
            assert threading.activeCount() == 1
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

''' 

  it "should allow and execute block continuations" do

    actor = Object.new
    name = Dramatis::Actor.new actor
    actor.should_receive(:foo).with(:bar).and_return(:foobar)

    result = nil
    retval = ( interface( name ).continue { |value| result = value } ).foo :bar
    retval.should be_nil
    result.should be_nil
    result.should be_nil # to perhaps highlight a threading problem

    Dramatis::Runtime.current.quiesce
    
    result.should equal( :foobar )

  end

  it "should execute messages to unbound names once bound" do

    name = Dramatis::Actor.new

    object = mock(Object.new)
    object.should_receive(:foo).with(:bar).and_return(:foobar)

    result = nil

    retval = ( interface( name ).continue { |value| result = value } ).foo :bar

    retval.should be_nil
    result.should be_nil

    Dramatis::Runtime.current.quiesce

    result.should be_nil

    interface( name ).bind object

    Dramatis::Runtime.current.quiesce

    result.should equal( :foobar )

  end

  it "rpc binds should return an actor name" do
    name = Dramatis::Actor.new
    retval = Dramatis.interface( name ).bind Hash.new
    retval.should be_kind_of( Dramatis::Actor::Name )
  end

  it "should be possible to bind with a non-rpc continuation" do
    name = Dramatis::Actor.new
    result = nil
    name = interface( name ).continue { |v| result = v }
    retval = interface( name ).bind Object.new
    retval.should equal( nil )
    result.should equal( nil )
    Dramatis::Runtime.current.quiesce
    result.should_not be_nil
  end

  it "should provide a url, if asked" do
    actor = Dramatis::Actor.new Object.new
    url = interface( actor ).url
    url.should match( %r[http://] )
  end

  it "unbound names should queue messages and deliver them in order"

  it "messages should be delivered out of order sometimes"

  it "flushing should guarantee message order"

end
'''
