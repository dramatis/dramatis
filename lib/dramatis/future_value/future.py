from __future__ import absolute_import

from traceback import print_stack
from traceback import format_stack

from logging import warning

import dramatis
from dramatis.future_value.interface import Interface as _Interface

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
        # warning( "".join(format_stack()) )
        super(FunctionProxy,self).__setattr__("_attr",attr)
        super(FunctionProxy,self).__setattr__("_continuation",continuation)

    def __call__(self,*args,**kwds):
        attr = super(FunctionProxy,self).__getattribute__("_attr")
        c = super(FunctionProxy,self).__getattribute__("_continuation")
        return c.value

_instmeth = type( FunctionProxy.__call__ )

from traceback import format_stack

def _func(): pass
_func = type(_func)

_wrap_desc = type(object.__getattribute__)

class Future(object):
    """proxy objects for the values made by calling future continuations
    
    dramatis.Futures are proxy objects for the values returned from
    actor method calls made with future continuations. When a method is
    called on a future, the runtime checks to see if the future has been
    evaluated and returned from the actor that executed the task. If it
    has, the method is executed on the returned value as if the proxy
    object was not there.

    If the task with the future continuation has not yet completed or
    the continuation task has not yet been run, the method called on
    the proxy is suspended until the reply is received. Thus, methods on
    futures sometimes but not always block. If they block, they have
    normal continuation gating semantics.

    dramatis.Future has no user-callable methods (except for the
    implicit forwarded methods). Other future operations are available
    through the dramatis.Future.Interface object, accessible via
    dramatis.interface."""

    def __init__(self,continuation):
        self._continuation = continuation

    def __str__(self):
        return self.__getattribute__(self,"__str__")

    def __add__(self,that):
        return dramatis.interface(self).value + that

    def __radd__(self,that):
        return that + dramatis.interface(self).value

    def __getattribute__(self,attr):
        c = super(Future,self).__getattribute__("_continuation")
        # warning( "c " + repr(c) + " attr " + attr)
        a = c._actor
        t = _func
        if a._behavior == None:
            return FunctionProxy(attr,c)

        # warning( "".join(format_stack()) )

        for out in ( a._behavior, ) + a._behavior.__class__.__mro__:
            # warning( "out" + repr( out) )
            d = None
            try:
                d = out.__dict__
            except AttributeError: pass
            desc = None
            if ( d ):
                desc = d.get( attr )
                # warning ("desc " + repr(desc))
            if ( desc ):
                # print repr(self), "x", repr(attr), type(desc)
                if ( type(desc) == property ):
                    return PropertyProxy(attr,c).__get__()
                elif ( type(desc) == _func ) \
                        or ( type(desc) == _instmeth ) \
                        or ( type(desc) == _wrap_desc ) \
                        :
                    return FunctionProxy(attr,c)
                else:
                    raise "hell: type? " + repr( type(desc) )
        # The attribute is not defined (at this time)
        # The only choice seems to be to assume it's a function
        return FunctionProxy(attr,c)

    Interface = _Interface
