from __future__ import absolute_import

from logging import warning

class Deadlock(Exception):

    def __init__( self, message = None, next = None ):
        # warning( "RAISING DEADLOCK" )
        super(Deadlock,self).__init__( message )
