class Interface(object):

    def __init__(self,actor):
        self._actor = actor

    def refuse( self, *args ):
        self._actor._gate.refuse( "object", *args )

    def accept( self, *args ):
        self._actor._gate.accept( "object", *args )

    def default( self, *args ):
        self._actor._gate.default( ( "object", ) + args )

    def always( self, args, value ):
        if not isinstance( args, tuple ):
            args = ( args, )
        self._actor._gate.always( ( ( "object", ) + args ), value )

    def enable_call_threading( self ):
        self._actor._set_call_threading_enabled(True)

    @property
    def name( self ):
        return self._actor.name

    def _gate( self ):
        return self._actor._gate
