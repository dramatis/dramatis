#!/usr/bin/env python

# cf. http://gee.cs.oswego.edu/dl/papers/fj.pdf

import inspect
import sys
import os.path

sys.path[0:0] = [ os.path.join( os.path.dirname( inspect.getabsfile( inspect.currentframe() ) ), '..', '..', 'lib' ) ]

from logging import warning

import dramatis

class Fib (dramatis.Actor):
    
    THRESHOLD = 13
  
    @property
    def value(self): return self._value

    def __init__(self,n):
      if n <= Fib.THRESHOLD:
        self._value = self.sequential( n )
      else:
        left = Fib( n - 1 )
        right = Fib( n - 2 )
        self._value = left.value + right.value

    def sequential(self,n):
        if n <= 1:
            return n
        else:
            return self.sequential(n-1) + self.sequential(n-2)

n = 28

print "fib("+str(n)+") = "+str(Fib(n).value)
