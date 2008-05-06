from __future__ import absolute_import

from logging import warning

from dramatis.actor.name import Name as _Name
from dramatis import Runtime
import dramatis.runtime as runtime
from dramatis.actor.interface import Interface as _Interface

class _Methods(type):
    pass

class Metaclass(_Methods):
    
    def __init__(cls,name,bases,dict):
        # warning( ["__init__", cls] )
        super(Metaclass,cls).__init__(cls,name,bases, dict)
        
    def __call__( cls, *args, **kwds ):
        # warning( ["__call__", cls,args,kwds] )
        if cls == Actor:
            return Actor.Name( runtime.Actor( *args,**kwds ) )
        else:
            # Hmmm ... this is a little heavy handed, but the only
            # thing I can think of (right now) if "actor" is to be
            # allowed in the constructor
            actor = runtime.Actor()
            interface = actor._interface
            name = Actor.Name( actor )
            behavior = cls.__new__( cls, *args, **kwds )
            class actor_class ( behavior.__class__ ):
                @property
                def actor( cls ):
                    return interface
            behavior.__class__ = actor_class
            # warning( "! " + str( behavior.__class__ ) )
            actor.bind( behavior )
            actor._gate.refuse( "object" )
            actor.actor_send( ( "object_initialize", ) + args, { "continuation": "rpc" } )
            # object = super(Metaclass,cls).__call__(*args,**kwds)
            # return Actor.Name( runtime.Actor( object ) )
            return name
        
class Actor(object):

    __metaclass__ = Metaclass

    Name = _Name
    Interface = _Interface

    class Methods(object):
        __metaclass__ = _Methods

