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

class Exc_Test:

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
