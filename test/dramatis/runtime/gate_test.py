#!/bin/env python

import inspect
import sys
import os.path
import threading

from logging import warning

sys.path[0:0] = [ os.path.join( os.path.dirname( inspect.getabsfile( inspect.currentframe() ) ), '..', '..', 'lib' ) ]

import dramatis.runtime.gate

class Gate_Test:

    def setup(self):
        self._gate = dramatis.runtime.Gate()

    def test(self):
        assert True
        
    def test_simple_defaults(self):
        "should accept simple defaults"

        assert self._gate.accepts( "actor" )
        assert self._gate.accepts( "continuation" )
        assert self._gate.accepts( "object" )
        assert self._gate.accepts( "foo" )


    def test_gate_list(self):
        "should return the gate list"

        spec = [ [("object",), True, None], 
                 [("continuation",), True, None],
                 [("actor",), True, None],
                 [(object,), True, None ] ]

        assert self._gate.list() == spec

    def test_simple_changes(self):
        "should obey simple changes"
        self._gate.refuse( "object" )
        assert not self._gate.accepts( "object" )
        assert self._gate.list() == [ [("object",), False, None], 
                                       [("continuation",), True, None],
                                       [("actor",), True, None],
                                       [(object,), True, None ] ]

    def test_reorder_on_change(self):
        "should reorder on change"
        self._gate.refuse( "actor" )
        assert not self._gate.accepts( "actor" )
        assert self._gate.list() == [ [("actor",), False, None],
                                       [("object",), True, None], 
                                       [("continuation",), True, None],
                                       [(object,), True, None ] ]



    def test_reorder_on_no_change(self):
        "should reorder on no change"
        self._gate.accept( "actor" )
        assert self._gate.accepts( "actor" )
        assert self._gate.list() == [ [("actor",), True, None],
                                       [("object",), True, None], 
                                       [("continuation",), True, None],
                                       [(object,), True, None ] ]

