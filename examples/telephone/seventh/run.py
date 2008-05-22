#!/usr/bin/env python

# THIS EXAMPLE ISN'T COMPLETE OR WORKING

# it deadlocks (don't remember)
# there's no "self" for main in python

import inspect
import sys
import os.path

sys.path[0:0] = [ os.path.join( os.path.dirname( inspect.getabsfile( inspect.currentframe() ) ), '..', '..', '..', 'lib' ) ]
sys.path[0:0] = [ os.path.join( os.path.dirname( inspect.getabsfile( inspect.currentframe() ) ), '..' ) ]

from logging import warning

import dramatis

from kid import Kid

tom = Kid( "Tom", self )
becky = Kid( "Becky", tom )
dick = Kid( "Dick", becky )
jane = Kid( "Jane", dick )
harry = Kid( "Harry", jane )
sally = Kid( "Sally", harry )

phrases = [ "his mom locked her keys in the car, " + \
            "so he should get a ride home with Hector",
            "Mac King is a comedy magic genius" ]

for phrase in phrases:
    print "Teacher:", phrase
    try:
        sally.whisper( phrase )
    except dramatis.Deadlock:
        warning( "woah: got a deadlock: that shouln't happen" )

for phrase in phrases:
    try:
        print "Teacher heard:", tom.ask()
    except dramatis.Deadlock:
        warning( "woah: got another deadlock" )
