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

class Ping (dramatis.Actor):

    def __init__(self,times,pong):
        self.pings_left = times
        dramatis.release( pong ).ping( self )

    def pong(self,caller):
        if self.pings_left % 1000 == 0:
            print "Ping: pong"
        if self.pings_left > 0:
            self.pings_left -= 1
            dramatis.release( caller ).ping( self )
            
class Pong (dramatis.Actor):

    def __init__(self):
        self.pong_count = 0

    def ping(self,caller):
        if self.pong_count % 1000 == 0:
            print "Pong: ping", self.pong_count
        self.pong_count += 1
        dramatis.release( caller ).pong( self )

pong = Pong()
ping = Ping( 1000, pong )
