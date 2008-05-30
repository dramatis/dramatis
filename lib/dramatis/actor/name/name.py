from __future__ import absolute_import

from logging import warning
from traceback import print_exc
from traceback import print_stack

from dramatis.actor.name.interface import Interface as _Interface

class PropertyProxy(object):
    def __init__(self,attr,actor,options):
        super(PropertyProxy,self).__setattr__("_attr",attr)
        super(PropertyProxy,self).__setattr__("_actor",actor)
        super(PropertyProxy,self).__setattr__("_options",options)

    def __get__(self,obj,type):
        actor = super(PropertyProxy,self).__getattribute__("_actor")
        attr = super(PropertyProxy,self).__getattribute__("_attr")
        options = super(PropertyProxy,self).__getattribute__("_options")
        return actor.object_send( "__getattribute__", (attr,), None, options )
        return obj.__getattribute__(self._name)

    def __call__(self,*args,**kwds):
        actor = super(PropertyProxy,self).__getattribute__("_actor")
        attr = super(PropertyProxy,self).__getattribute__("_attr")
        options = super(PropertyProxy,self).__getattribute__("_options")
        return actor.object_send( attr, args, kwds, options )

class FunctionProxy(object):
    def __init__(self,attr,actor,options):
        super(FunctionProxy,self).__setattr__("_attr",attr)
        super(FunctionProxy,self).__setattr__("_actor",actor)
        super(FunctionProxy,self).__setattr__("_options",options)

    def __call__(self,*args,**kwds):
        actor = super(FunctionProxy,self).__getattribute__("_actor")
        attr = super(FunctionProxy,self).__getattribute__("_attr")
        options = super(FunctionProxy,self).__getattribute__("_options")
        return actor.object_send( attr, args, kwds, options )

_instmeth = type( FunctionProxy.__call__ )

def _func(): pass
_func = type(_func)

class Name(object):

    def __init__(self,actor):
        super(Name,self).__setattr__("_actor",actor)
        super(Name,self).__setattr__("_options",{"continuation":"rpc"})

    def __call__(self,*args,**kwds):
        return self.__getattribute__("__call__")(*args,**kwds)

    def __lshift__(self,*args,**kwds):
        return self.__getattribute__("__lshift__")(*args,**kwds)

    def __getattribute__(self,attr):
        # logging.warning(FunctionProxy)
        a = super(Name,self).__getattribute__("_actor")
        o = super(Name,self).__getattribute__("_options")
        t = _func
        if o.has_key( "continuation_send" ):
            return FunctionProxy(attr,a,o)
        if a._behavior == None:
            return FunctionProxy(attr,a,o)

        # warning( "a: " + str(a) )
        # warning( "list: " + str( ( a._behavior, ) + a._behavior.__class__.__mro__ ) )
        for out in ( a._behavior, ) + a._behavior.__class__.__mro__:
            # print
            # print repr(out), out.__dict__
            d = None
            try:
                d = out.__dict__
            except AttributeError: pass
            # raise AttributeError( "'" + str(out.__class__.__name__) + "' object has no attribute '" + attr + "'" )
            desc = None
            if ( d ):
                desc = d.get( attr )
            if ( desc ):
                # print repr(self), "x", repr(attr), type(desc)
                if ( type(desc) == property ):
                    return PropertyProxy(attr,a,o).__get__(o,type(o))
                elif ( type(desc) == _func ) or \
                      ( type(desc) == _instmeth ):
                    return FunctionProxy(attr,a,o)
                elif ( str(type(desc)) == "<type 'wrapper_descriptor'>" ):
                    return FunctionProxy(attr,a,o)
                else:
                    raise "hell: type? " + str( type(desc) )
        # The attribute is not defined (at this time)
        # The only choice seems to be to assume it's a function
        return FunctionProxy(attr,a,o)


    Interface = _Interface
