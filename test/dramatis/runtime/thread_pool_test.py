#!/bin/env python

import inspect
import sys
import os.path


sys.path[0:0] = [ os.path.join( os.path.dirname( inspect.getabsfile( inspect.currentframe() ) ), '..', '..' ) ]

sys.path[0:0] = [ os.path.join( os.path.dirname( inspect.getabsfile( inspect.currentframe() ) ), '..', '..', '..', 'lib' ) ]

from logging import warning
import time
import threading

from test_helper import DramatisTestHelper

import dramatis.runtime

class Thread_Pool_Test ( DramatisTestHelper ):

    def teardown(self):
        self.runtime_check()

    def test_resetable(self):
        "it should be resetable"
        thread_pool = dramatis.runtime.ThreadPool()
        thread_pool.reset()

    def test_allocate_and_do_stuff(self):
        "it should allocate threads that do stuff" 

        thread_pool = dramatis.runtime.ThreadPool()
        x = [1]
        def f():
            time.sleep( 0.1 )
            x[0] += 1
        t = thread_pool(f)
        assert x[0] == 1
        assert thread_pool.length == 0
        time.sleep( 0.2 )
        assert x[0] == 2
        assert thread_pool.length == 1
        thread_pool.reset()
        assert thread_pool.length == 0

    def test_wait_checkin(self):
        "it should wait for all thread to be checked in"
        thread_pool = dramatis.runtime.ThreadPool()
        def f(v):
            time.sleep( v )
        thread_pool(f,[0.1])
        assert thread_pool.length == 0
        assert thread_pool.size == 1
        time.sleep( 0.2 )
        assert thread_pool.length == 1
        assert thread_pool.size == 1
        thread_pool(f,[0.1])
        assert thread_pool.length == 0
        assert thread_pool.size == 1
        thread_pool(f,[0.3])
        assert thread_pool.length == 0
        assert thread_pool.size == 2
        time.sleep( 0.2 )
        assert thread_pool.length == 1
        assert thread_pool.size == 2
        thread_pool.reset()
        assert thread_pool.length == 0
        assert thread_pool.size == 0

    def test_soft_reset(self):
        "it shouldn't wait if soft reset"
        thread_pool = dramatis.runtime.ThreadPool()
        def f(v):
            time.sleep(v)
        thread_pool(f,[0.1])
        assert thread_pool.length == 0
        assert thread_pool.size == 1
        time.sleep( 0.2 )
        assert thread_pool.length == 1
        assert thread_pool.size == 1
        thread_pool(f,[0.1])
        assert thread_pool.length == 0
        assert thread_pool.size == 1
        thread_pool(f,[0.3])
        assert thread_pool.length == 0
        assert thread_pool.size == 2
        time.sleep( 0.2 )
        assert thread_pool.length == 1
        assert thread_pool.size == 2
        thread_pool.reset( True )
        assert thread_pool.length == 0
        assert thread_pool.size == 1
        thread_pool.reset()
        assert thread_pool.length == 0
        assert thread_pool.size == 0
