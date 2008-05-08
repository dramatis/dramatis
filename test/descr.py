#!/usr/bin/env python

class Real(object):
    def method(self):
        return "method"
    @property
    def prop(self):
        # print "prop called"
        return "prop"

class PropertyProxy(object):
    def __init__( self, object, name ):
        self._object = object
        self._name = name
    def __get__(self,obj,type):
        # print self, "__get__", obj, type
        return obj.__getattribute__(self._name)

class FunctionProxy(object):
    def __init__( self, object, name ):
        self._object = object
        self._name = name
    def __call__( self, *args, **kwds ):
        return self._object.__getattribute__( self._name ).__call__( *args, **kwds )

def _func(): pass
_func = type(_func)

class Proxy(object):
    def __init__( self, object ):
        self.__object = object

    def __getattribute__( self, name ):
        o = super(Proxy,self).__getattribute__( "_Proxy__object" )
        if name == "_Proxy__object":
            return o
        t = type( type(o).__dict__[ name ] )
        if t == property:
            return PropertyProxy( self.__object, name ).__get__(o,type(o))
        elif t == _func:
            return FunctionProxy( self.__object, name )
        else: raise "hell"

r = Real()
p = Proxy( r )

m = p.method
# print m
# print m()
assert m() == "method"

prop = p.prop
# print prop
assert prop == "prop"
