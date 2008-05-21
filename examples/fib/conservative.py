#!/usr/bin/env python

# cf. http://gee.cs.oswego.edu/dl/papers/fj.pdf

import math
import time
import inspect
import sys
import os.path

sys.path[0:0] = [ os.path.join( os.path.dirname( inspect.getabsfile( inspect.currentframe() ) ), '..', '..', 'lib' ) ]

from logging import warning

import threading

import dramatis

THREAD_LEVELS = None

class Fib (dramatis.Actor):
    
    @property
    def value(self): return self._value

    def __init__(self, n, level = None ):
        super(Fib,self).__init__(self)
        if level  == None:
            level = THREAD_LEVELS
        self.actor.refuse("value")
        dramatis.release( self.actor.name ).calc( n, level )

    def calc(self, n, level ):
        if level == 0:
            print threading.currentThread(), "start"
            now = time.time()
            self._value = self.sequential(n)
            print "sequential(%d)" % n, time.time() - now
        else:
            left = Fib( n - 1, level - 1 )
            right = Fib( n - 2, level - 1 )
            self._value = left.value + right.value
        self.actor.accept( "value" )

    def sequential(self,n):
        if n <= 1:
            return n
        else:
            return self.sequential(n-1) + self.sequential(n-2)

n = None
try:
    n = int(sys.argv[1])
except: pass
if not ( n and n > 0):
    n = 36

threads = None
try:
    threads = int(sys.argv[2])
except: pass
if not (threads and threads > 1):
    threads = 1

THREAD_LEVELS = math.ceil( ( math.log(threads)/math.log(2) ) )

print "fib(%d) = %d" % ( n, Fib(n).value )
  
