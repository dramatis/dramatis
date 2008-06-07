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

_first_line = []
_second_line = []

sys.path[0:0] = [ os.path.join( os.path.dirname( inspect.getabsfile( inspect.currentframe() ) ), '..' ) ]
from test_helper import DramatisTestHelper

class Exc_Test ( DramatisTestHelper ):

    def teardown(self):
        self.runtime_check()

    def test_pretty_exc(self):
        class Foo ( dramatis.Actor ):
            def foo(self, that):
                _first_line[:] = getframeinfo( currentframe() )[0:2]; _first_line[1] += 1
                return that.bar()
        class Bar ( dramatis.Actor ):
            def bar(self):
                _second_line[:] = getframeinfo( currentframe() )[0:2]; _second_line[1] += 1
                a, b = [ 1, 2, 3]
                return "foobar"
        foo = Foo()
        bar = Bar()
        okay = False
        try:
            foo.foo(bar)
        except ValueError, ve:
            tb = dramatis.error.traceback( ve )
            assert list(tb[-1][0:2]) == _second_line
            assert list(tb[-2][0:2]) == _first_line
            okay = True
        assert okay
