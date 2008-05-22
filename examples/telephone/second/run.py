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

phrase = "his mom locked her keys in the car, " + \
         "so he should get a ride home with Hector"

print "Teacher:", phrase
heard = sally.whisper( phrase )
print "Teacher heard:", heard


