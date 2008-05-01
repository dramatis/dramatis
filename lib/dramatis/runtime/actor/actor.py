from __future__ import with_statement

from threading import Lock

import dramatis
from dramatis.runtime import Task
from dramatis.runtime import Gate
from dramatis.runtime import Scheduler

class Actor(object):

    def __init__(self,object = None):
        self.mutex = Lock()
        self.call_threading = False
        self.call_thread = None
        self.object = object
        self.gate = Gate()
        if not object:
            self.gate.refuse("object")
        self.gate.always( ( [ "object", "dramatis_exception" ] ), True )
        self.block()
        self.queue = []
        self.mutex = Lock()
        self.continuations = {}
        self.object_interface = dramatis.Actor.Interface(self)
        Scheduler.current.append( self )

    @property
    def runnable(self):
        return self.state == "runnable"

    def is_blocked(self):
        return self.state == "blocked"

    def block(self):
        self.state = "blocked"

    def current_call_thread(self,that):
        return self.call_thread and self.call_thread == that

    def object_send(self,name,args,kwds,opts):
        t = None
        o = opts.get("continuation_send")
        if o:
            t = "continuation"
            args.unshift(o)
        else:
            t = "object"
        return self.common_send( t, (name,)+args, opts )

    def common_send(self,dest,args,opts):

        task = Task( self, dest, args, opts  )

        with self.mutex:
            if ( not self.runnable and \
                 ( self.gate.accepts(  *( ( task.type, task.method ) + task.arguments ) ) or self.current_call_thread( task.call_thread ) ) ):
                self.runnable = true
                Scheduler.current.schedule( task )
            else:
                self.queue.append(task)

        return task.queued()
