class Deadlock(Exception):

    def __init__( self, message = None, next = None ):
        super(Deadlock,self).__init__( message )
