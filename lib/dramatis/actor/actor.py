from __future__ import absolute_import

import logging

from dramatis.actor.name import Name as _Name
from dramatis import Runtime
import dramatis.runtime as runtime
from dramatis.actor.interface import Interface as _Interface

class _Methods(type):
    pass

class Metaclass(_Methods):
    
    def __init__(cls,name,bases,dict):
        # logging.warning( ["__init__", cls] )
        super(Metaclass,cls).__init__(cls,name,bases, dict)
        
    def __call__( cls, *args, **kwds ):
        # logging.warning( ["__call__", cls,args,kwds] )
        if cls == Actor:
            return Actor.Name( runtime.Actor( *args,**kwds ) )
        else:
            object = super(Metaclass,cls).__call__(*args,**kwds)
            return Actor.Name( runtime.Actor( object ) )

class Actor(object):

    __metaclass__ = Metaclass

    Name = _Name
    Interface = _Interface

    class Methods(object):
        __metaclass__ = _Methods

