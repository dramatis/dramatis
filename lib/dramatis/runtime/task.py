from __future__ import with_statement

from logging import warning
from threading import Lock

import dramatis
import dramatis.runtime.continuation
from dramatis.runtime import Scheduler

class Task(object):

    @property
    def dest(self):
        return self._dest

    @property
    def method(self):
        return self._args[0]

    @property
    def arguments(self):
        return self._args[1:]

    @property
    def call_thread(self):
        return self._call_thread

    actor = property( lambda(self): self._actor )

    def __init__(self, actor, dest, args, options ):
        self._actor = actor
        self._dest = dest
        self._args = tuple(args)
        self._options = options

        self._call_thread = None

        name = Scheduler.actor
        actor = super(dramatis.Actor.Name,name).__getattribute__("_actor")

        behavior = actor.behavior
        args = self._args
        for i in xrange(len(args)):
            if( args[i] is behavior ):
                args[i] = name

        if( actor.call_threading_enabled ):
            if( self._options[:call_thread] and
                actor._call_thread and
                self._options[:call_thread] != actor._call_thread ):
                raise "hell"
            self._call_thread = actor._call_thread
            if( self._call_thread == None ):
                self._call_thread = self.to_s

        # warn "task #{self} #{_args[0]} call thread [ #{self._call_thread} ] #{options.to_a.join(' ')}"

        if ( self._options["continuation"] == "none" ):
            self._continuation = dramatis.runtime.continuation.Nil( name, self._call_thread )
        elif( self._options["continuation"] == "rpc" ):
            self._continuation = dramatis.runtime.continuation.RPC( name, self._call_thread )
        elif( self._options["continuation"] == "future" ):
            self._continuation = dramatis.runtime.continuation.Future( name, self._call_thread )
#        when Proc
#        self._continuation = dramatis.runtime.continuation.Proc( name,  self._call_thread, options[:continuation], \
#                                                                             options[:exception] )
        else:
            raise dramatis.Internal( "invalid contiunation type" )

    def exception(self, e):
        return self._continuation.exception( e )

    def queued(self):
        return self._continuation.queued()

    def deliver(self):
        return self._actor.deliver( self._dest,
                                     self._args,
                                     self._continuation,
                                     self._call_thread )


