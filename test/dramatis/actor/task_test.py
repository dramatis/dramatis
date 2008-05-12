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

class Task_Test:

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

    def test_return_errors_to_caller(self):
        "should return errors to calling actor even when non-rpc (non-main)"
        class callerClass(dramatis.Actor):
            def __init__(self):
                self._exception = None
            def caller(self, callee):
                dramatis.release( callee ).callee()
            def dramatis_exception(self,e):
                self._exception = e
            def exception(self):
                if self._exception: raise self._exception
        caller = callerClass()
        callee = dramatis.Actor( object() )

        caller.caller( callee )

        dramatis.Runtime.current.quiesce()

        okay = False
        try:
            caller.exception()
        except AttributeError: okay = True

'''
  it "should do something reasonable when the caller is main" do
    callee = Dramatis::Actor.new Object.new
    lambda { callee.callee }.should raise_error( NoMethodError )
    release( callee ).callee
    Dramatis::Runtime.current.warnings = false
    lambda { Dramatis::Runtime.current.quiesce }.should raise_error( Dramatis::Error::Uncaught )
    Dramatis::Runtime.current.warnings = true
    Dramatis::Runtime.current.exceptions.length.should equal( 1 )
    Dramatis::Runtime.current.clear_exceptions
  end

  it "should default to global when no dramatis_exception defined" do
    callerClass = Class.new do
      include Dramatis::Actor
      def caller callee
        release( callee ).callee
      end
    end

    caller = callerClass.new
    callee = Dramatis::Actor.new Object.new

    Dramatis::Runtime.current.warnings = false
    caller.caller callee
    lambda { Dramatis::Runtime.current.quiesce }.should raise_error( Dramatis::Error::Uncaught )
    Dramatis::Runtime.current.warnings = true

    lambda { raise Dramatis::Runtime.current.exceptions[0] }.should raise_error( NoMethodError )
    Dramatis::Runtime.current.clear_exceptions
      
  end

  it "should call dramatis_exception on main if that works(?)"

end

'''
