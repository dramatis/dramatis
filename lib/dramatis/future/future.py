from __future__ import absolute_import

from traceback import print_stack
from traceback import format_stack

from logging import warning

from dramatis.future.interface import Interface as _Interface

class PropertyProxy(object):
    def __init__(self,attr,continuation):
        super(PropertyProxy,self).__setattr__("_attr",attr)
        super(PropertyProxy,self).__setattr__("_continuation",continuation)

    def __get__(self):
        attr = super(PropertyProxy,self).__getattribute__("_attr")
        c = super(PropertyProxy,self).__getattribute__("_continuation")
        return c.value.__getattribute__( attr )

    def __call__(self,*args,**kwds):
        attr = super(PropertyProxy,self).__getattribute__("_attr")
        c = super(PropertyProxy,self).__getattribute__("_continuation")
        return c.value.__call__( attr, args, kwds )

class FunctionProxy(object):
    def __init__(self,attr,continuation):
        warning( "".join(format_stack()) )
        super(FunctionProxy,self).__setattr__("_attr",attr)
        super(FunctionProxy,self).__setattr__("_continuation",continuation)

    def __call__(self,*args,**kwds):
        attr = super(FunctionProxy,self).__getattribute__("_attr")
        c = super(FunctionProxy,self).__getattribute__("_continuation")
        return c.value.__call__( attr, args, kwds )

_instmeth = type( FunctionProxy.__call__ )

from traceback import format_stack

def _func(): pass
_func = type(_func)

class Future(object):

    def __init__(self,continuation):
        self._continuation = continuation

    def __getattribute__(self,attr):
        c = super(Future,self).__getattribute__("_continuation")
        warning( "c " + str(c) + " attr " + attr)
        a = c._actor
        t = _func
        if a._behavior == None:
            return FunctionProxy(attr,c)

        warning( "".join(format_stack()) )

        for out in ( a._behavior, ) + a._behavior.__class__.__mro__:
            warning( "out" + str( out) )
            d = None
            try:
                d = out.__dict__
            except AttributeError: pass
            desc = None
            if ( d ):
                desc = d.get( attr )
                warning ("desc " + str(desc))
            if ( desc ):
                # print repr(self), "x", repr(attr), type(desc)
                if ( type(desc) == property ):
                    return PropertyProxy(attr,c).__get__()
                elif ( type(desc) == _func ) or \
                      ( type(desc) == _instmeth ):
                    return FunctionProxy(attr,c)
                else:
                    raise "hell: type? " + str( type(desc) )
        # The attribute is not defined (at this time)
        # The only choice seems to be to assume it's a function
        return FunctionProxy(attr,c)

    Interface = _Interface
