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
            assert threading.activeCount() == \
                    1 + len(dramatis.runtime.Scheduler.ThreadPool)
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

    def test_main_caller(self):
        "should do something reasonable when the caller is main"
        callee =dramatis.Actor( object() )
        okay = False
        try:
            callee.callee()
            raise Exception("should not be reached")
        except AttributeError: okay = True
        assert okay
        dramatis.release( callee ).callee()
        dramatis.Runtime.current.warnings = False
        okay = False
        try:
            dramatis.Runtime.current.quiesce()
        except dramatis.error.Uncaught: okay = True
        assert okay
        dramatis.Runtime.current.warnings = True
        assert len(dramatis.Runtime.current.exceptions() ) == 1
        dramatis.Runtime.current.clear_exceptions()


    def test_global_exc(self):
        "should default to global when no dramatis_exception defined" 
        class callerClass (dramatis.Actor):
            def caller(self, callee):
                dramatis.release( callee ).callee()

        caller = callerClass()
        callee = dramatis.Actor( object() )

        dramatis.Runtime.current.warnings = False
        caller.caller( callee )

        okay = False
        try:
            dramatis.Runtime.current.quiesce()
        except dramatis.error.Uncaught: okay = True
        assert okay
        dramatis.Runtime.current.warnings = True

        okay = False
        try:
            raise dramatis.Runtime.current.exceptions()[0]
        except AttributeError: okay = True
        dramatis.Runtime.current.clear_exceptions()
      
    def test_call_d_e_on_main(self):
        "should call dramatis_exception on main if that works(?)"
