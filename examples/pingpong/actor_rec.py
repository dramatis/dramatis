#!/usr/bin/env python

# cf. Scala by Example, Chapter 3

import time
import random
import inspect
import sys
import os.path

sys.path[0:0] = [ os.path.join( os.path.dirname( inspect.getabsfile( inspect.currentframe() ) ), '..', '..', 'lib' ) ]

from logging import warning

import dramatis

class PingPong ( dramatis.Actor ):

    def __init__(self,name):
        self.actor.enable_call_threading()
        self._name = name

    def pingpong(self,count,partner):
        if count == 0:
            print "%s: done" % self._name
        else:
            if count % 500 == 0 or count % 500 == 1:
                print "%s: pingpong %d" % ( self._name, count )
            partner.pingpong( count-1, self )
            # sleep 0.001

ping = PingPong( "ping" )
pong = PingPong( "pong" )

ping.pingpong( int(sys.argv[1]), pong )
