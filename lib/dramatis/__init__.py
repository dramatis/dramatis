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
    interface = None
    try:
        interface = type(object).Interface
    except AttributeError, error:
        raise dramatis.error.Interface(  "object is not a dramatis interfacable object" )
    return interface( object, *args, **kwds )

def release( name ):
    return interface( name ).continuation( None )

def future( name ):
    return interface( name ).future()
