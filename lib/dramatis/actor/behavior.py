from __future__ import absolute_import

from logging import warning

import dramatis
from dramatis.actor.name import Name as _Name
from dramatis import Runtime
import dramatis.runtime as runtime
from dramatis.actor.interface import Interface as _Interface

class Metaclass(type):
    
    def __init__(cls,name,bases,dict):
        super(Metaclass,cls).__init__(cls,name,bases, dict)
        
    def __call__( cls, *args, **kwds ):
        # See also actor comments ...
        interface = dramatis.Actor.Interface( None )
        behavior = cls.__new__( cls, *args, **kwds )
        class actor_class ( behavior.__class__ ):
            @property
            def actor( cls ):
                return interface
        behavior.__class__ = actor_class
        behavior.__init__( *args, **kwds )
        return behavior
        
class Behavior(object):
    """Class used as the base of actor behavior classes.

    Creating an actor behavior does not create an actor but creates an
    object that an actor can "become" while still providing the actor
    method hook or value-add operations."""

    __metaclass__ = Metaclass
