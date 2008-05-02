from __future__ import with_statement

from logging import warning
from threading import Lock

from traceback import print_exc

import dramatis
import dramatis.runtime

class Actor(object):

    def __init__(self,behavior = None):
        self._call_threading_enabled = False
        self._call_thread = None
        self._behavior = behavior
        self._gate = dramatis.runtime.Gate()
        if not behavior:
            self._gate.refuse("object")
        self._gate.always( ( [ "object", "dramatis_exception" ] ), True )
        self.block()
        self._queue = []
        self._mutex = Lock()
        self._continuations = {}
        self._interface = dramatis.Actor.Interface(self)
        dramatis.runtime.Scheduler.current.append( self )

    @property
    def name(self):
        if( not hasattr(self,"_name") ):
            self._name = dramatis.Actor.Name( self )
        return self._name

    @property
    def runnable(self):
        return self.state == "runnable"

    behavior = property( lambda(self): self._behavior )

    def _set_call_threading_enabled( self, v ):
        self._call_threading_enabled = v

    call_threading_enabled = property(
        lambda(self): self._call_threading_enabled,
        lambda(self,v): self._set_call_threading_enabled(v) )

    def make_runnable(self):
        self.state = "runnable"

    def is_blocked(self):
        return self.state == "blocked"

    def block(self):
        self.state = "blocked"

    def current_call_thread(self,that):
        return self._call_thread and self._call_thread == that

    def actor_send( self, args, opts ):
        self.common_send( "actor", args, opts )

    def object_send(self,name,args,kwds,opts):
        t = None
        o = opts.get("continuation_send")
        if o:
            t = "continuation"
            args = (o,)+args
        else:
            t = "object"
        return self.common_send( t, (name,)+args, opts )

    def common_send(self,dest,args,opts):

        warning( "common send " + dest + " " + str(args) + " " + str(opts) )

        task = dramatis.runtime.Task( self, dest, args, opts  )

        with self._mutex:
            if ( not self.runnable and
                 ( self._gate.accepts(  *( ( task.dest, task.method ) + task.arguments ) ) or self.current_call_thread( task.call_thread ) ) ):
                self.make_runnable
                dramatis.runtime.Scheduler.current.schedule( task )
            else:
                self._queue.append(task)

        return task.queued()

    def deliver( self, dest, args, continuation, call_thread ):
        old_call_thread = self._call_thread
        try:
            self._call_thread = call_thread
            method = args[0]
            args = args[1:]
            result = None
            warning( "deliver " + dest + " " + method + " " + str(args) )
            if ( dest == "actor" ):
                result = self.__getattribute__(method).__call__( *args )
            elif ( dest == "object" ):
                v = self._behavior.__getattribute__(method).__call__( *args )
                if v is self._behavior:
                    v = self.name()
                result = v
            elif ( dest == "continuation" ):
                continuation_name = method
                c = self._continuations[continuation_name]
                if not c: raise "hell 0 #{Thread.current}"
                method = args[0]
                args = args[1:]
                if( method == "result" ):
                    method = "continuation_result"
                elif( method == "exception" ):
                    method = "continuation_exception"
                else: raise "hell *"
                c.__getattribute__(method).__call__(*args)
                self._continuations.delete( continuation_name )
            else: raise "hell 1: " + str(self._dest)
            warning("after y" + repr(continuation))
            continuation.result( result )
            warning("after z")
        except Exception, exception:
            try:
                warning( "trying to except " + repr(exception) )
                print_exc()
                continuation.exception( exception )
            except Exception, e:
                warning( "double exception fault: " + repr(e) )
                print_exc()
                raise e
        finally:
            self._call_thread = old_call_thread
            self.schedule

    def deadlock( self, exception ):
        tasks = []
        with self._mutex:
            tasks = list(self._queue)
            self._queue[:] = []
        for task in tasks:
            task.exception( exception )

    def register_continuation( self, c ):
        self._continuations[str(c)] = c

    def schedule( self, continuation = None ):
        with self._mutex:
            task = None
            index = 0
            while task == None and index < len(self._queue):
                candidate = self._queue[index]
                if( self._gate.accepts( *( [ candidate.type, candidate.method ] + candidate.arguments ) ) or 
                    self.current_call_thread( candidate.call_thread ) ):
                    task = candidate
                    self._queue.pop(index)
                index += 1
            if( task ):
                dramatis.runtime.Scheduler.current.schedule( task )
            else:
                self.block()

