"""Provides methods to manipulate dramatis objects.

Each function can be accessed as a module function, e.g.,
   Dramatis.interface(...)
"""

from __future__ import absolute_import

from logging import warning

import dramatis.error
Error = dramatis.error.Error

import dramatis.runtime
Runtime = dramatis.runtime.Runtime

import dramatis.actor
Actor = dramatis.actor.Actor

import dramatis.deadlock
Deadlock = dramatis.deadlock.Deadlock

import dramatis.future_value
Future = dramatis.future_value.future.Future

def interface( object, *args, **kwds ):
    """
    Return an interface object for the given proxy object.

    Takes a dramatis proxy object and returns an object that can be
    used to operate directly on the proxy, rather than on the proxied
    object. Since dramatis objects like actor names and futures are
    proxy objects, normal method calls on them are directed to the
    proxied object. In order to perform operations on the proxies
    themselves, the interface method is used to get access to a
    non-proxy object. If the object passed is a dramatis.Actor.Name,
    the result is a dramatis.Actor.Name.Interface object. If the
    object passed is a dramatis.Future, the result is a
    dramatis.Future.Interface object.
    """

    interface = None
    try:
        interface = type(object).Interface
    except AttributeError, error:
        raise dramatis.error.Interface(  "object is not a dramatis interfacable object" )
    return interface( object, *args, **kwds )

def release( name ):
    """Return an actor name with asynchronous semantics.

    Takes an actor name and returns a new actor name which, when used
    as the target of a method call, will pass a null continuation. As
    a result, the call will not block or otherwise wait for a
    result. The result of such a call is always nil."""

    return interface( name ).continuation( None )

def future( name ):
    """Return an actor name with future semantics.

    Takes an actor name and returns a new actor name which, when used
    as the target of a method call, will pass a future
    continuation. It immediately returns a dramatis.Future object."""

    return interface( name ).future()
