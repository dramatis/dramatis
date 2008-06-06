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
        """create a new actor

        DerivedClass( *args, &block ) -> an_actor_name
        Actor( behavior = nil ) -> an_actor_name

        The first case is used when a class has derived from
        dramatis.Actor. In this case, the arguments are passed to the 
        initialize of method of the including class like normal.

        The second case is used when creating so called <em>naked
        actors</em>, e.g.,
        my_hash = dramatis.Actor( dict() )
        If no
        behavior is provided, the actor can be later bound to a behavior
        by calling dramatis.Actor.Name.Interface.bind

        In all cases, new returns a dramatis.Actor.Name proxy
        object.
        """

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
                    """provide access to the interface object for this actor

                    self.actor provides classes that have derived from
                    dramatis.Actor access to a dramatis.Actor.Interface
                    object by which they can access
                    their actor name and other actor operations."""
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
    """
    Class used as the base of actor classes.

    The dramatis.Actor class is used as a base of actor classes and
    objects. An actor class can be created by deriving from dramatis.Actor, e.g.,
    class MyClass ( dramatis.Actor ):
       ...

    or can be used to create so called _naked_ _actors_, e.g.,
    my_hash_actor = dramatis.Actor( dict() )

    When used as a base class, dramatis.Actor has two effects:
    1. It causes __new__ to return a dramatis.Actor.Name rather than an object reference
    1. It defines an actor method which can be used by the class to access its actor name and
    otherwise affect its actor semantics
    """

    __metaclass__ = Metaclass

    Name = _Name
    Interface = _Interface

    class Methods(object):
        __metaclass__ = _Methods

