#!/usr/bin/env python

import inspect
import sys
import os.path

sys.path[0:0] = [ os.path.join( os.path.dirname( inspect.getabsfile( inspect.currentframe() ) ), '..', '..', '..', 'lib' ) ]
sys.path[0:0] = [ os.path.join( os.path.dirname( inspect.getabsfile( inspect.currentframe() ) ), '..' ) ]

from kid import Kid

tom = Kid( "Tom" )
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
    sally.whisper( phrase )

for phrase in phrases:
    print "Teacher heard:", tom.ask()
