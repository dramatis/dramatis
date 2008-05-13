from __future__ import with_statement

from logging import warning

from threading import Lock
from threading import Condition

from traceback import print_exc

import dramatis
from dramatis.runtime import Scheduler

class Nil(object):

    def __init__(self,name,call_thread):
        self._name = name

    def queued(self): pass

    def result(self, result): pass

    def exception( self, exception ):
        try:
            dramatis.interface( dramatis.release( self._name ) ).exception( exception )
        except Exception, e:
            print_exc()
            raise e
    
class RPC(object):

    def __init__(self,name,call_thread):
        self._state = "start"
        self._mutex = Lock()
        self._wait = Condition( self._mutex )
        self._call_thread = call_thread
        self._actor = \
            dramatis.interface( Scheduler.actor ).\
              _continuation( self, { call_thread: call_thread } )

    '''
      def actor
        @actor.instance_eval { @actor }
      end
    '''

    def queued(self):

        with self._mutex:
            if( self._state == "start" ):
                self._state = "waiting"
                actor = super(dramatis.Actor.Name,self._actor).\
                    __getattribute__("_actor")
                try:
                    tag = str(self)
                    call_thread = self._call_thread
                    actor._call_thread = call_thread
                    actor._gate.only( [ "continuation", tag ], { "tag": tag } )
                    actor.schedule( self )
                    Scheduler.current.suspend_notification( self )
                    self._wait.wait()
                    # this causes a deadlock if the waking thread,
                    # which may be retiring, does so before this
                    # thead has awakend and notified the scheduler
                    # sleep 1
                finally:
                    tag = str(self)
                    actor._gate.default_by_tag( tag )
                if( self._state != "done" ):
                    raise "hell"

        if( self._type == "return" ):
            return self._value
        elif( self._type == "exception" ):
            # if isinstance( self._value, dramatis.Deadlock ):
            #     self._value = dramatis.Deadlock( None, next = self._value )
            #   # self._value.set_traceback()
            dramatis.error.traceback( self._value )
            raise self._value

    def result( self, result ):
        # warning( "result " + str(result) + " " + str(self._actor) )
        self._actor.result( result )

    def exception( self, exception ):
        self._actor.exception( exception )

    def continuation_result( self, result ):
        # warning( "c result " + str(result) )
        with self._mutex:
            self._type = "return"
            self._value = result
            if self._state == "start":
                self._state = "signaled"
            else:
                self._state = "done"
                dramatis.runtime.Scheduler.current.wakeup_notification( self )
                self._wait.notify()

    def continuation_exception( self, exception ):
        with self._mutex:
            self._type = "exception"
            self._value = exception
            if self._state == "start":
                self._state = "signaled"
            else:
                self._state = "done"
                dramatis.runtime.Scheduler.current.wakeup_notification( self )
                self._wait.notify()

class Block( object ):

    def __init__(self, name, call_thread, result, exception):
        # p "p.n #{call_thread} #{result} #{except}"
        self._result_block = result
        self._exception_block = exception
        self._name = name
        self._continuation = \
            dramatis.interface( dramatis.runtime.Scheduler.actor ) \
              ._continuation( self, { call_thread: call_thread } )

    def queued(self): pass
    
    def result(self, result):
        self._continuation.result( result )
    
    def exception(self, exception):
        self._continuation.exception( exception )

    def continuation_result(self, result):
        self._result_block( result )

    def continuation_exception(self, exception):
        if self._exception_block:
            self._exception_block( exception )
        else:
            dramatis.release( self._name ).dramatis_exception( exception )

class Future(object):

    def __init__(self,name,call_thread):
        self._state = "start"
        self._mutex = Lock()
        self._wait = Condition( self._mutex )
        self._call_thread = call_thread
        self._actor = \
            dramatis.interface( Scheduler.actor ).\
              _continuation( self, { call_thread: call_thread } )

    @property
    def ready(self):
        with self._mutex:
            return self._state == "done" or self._state == "signaled"

    def queued(self):
        return dramatis.Future( self )

    @property
    def value(self):
        with self._mutex:
            if( self._state == "start" ):
                self._state = "waiting"
                actor = super(dramatis.Actor.Name,self._actor).\
                    __getattribute__("_actor")
                try:
                    tag = str(self)
                    call_thread = self._call_thread
                    actor._call_thread = call_thread
                    actor._gate.only( [ "continuation", tag ], { "tag": tag } )
                    actor.schedule( self )
                    Scheduler.current.suspend_notification( self )
                    self._wait.wait()
                    # this causes a deadlock if the waking thread,
                    # which may be retiring, does so before this
                    # thead has awakend and notified the scheduler
                    # sleep 1
                finally:
                    tag = str(self)
                    actor._gate.default_by_tag( tag )
                if( self._state != "done" ):
                    raise "hell"

        if( self._type == "return" ):
            return self._value
        elif( self._type == "exception" ):
            if isinstance( self._value, dramatis.Deadlock ):
                self._value = dramatis.Deadlock( None, next = self._value )
                # self._value.set_traceback()
            raise self._value

    def result( self, result ):
        # warning( "result " + str(result) + " " + str(self._actor) )
        self._actor.result( result )

    def exception( self, exception ):
        self._actor.exception( exception )

    def continuation_result( self, result ):
        # warning( "c result " + str(result) )
        with self._mutex:
            self._type = "return"
            self._value = result
            if self._state == "start":
                self._state = "signaled"
            else:
                self._state = "done"
                dramatis.runtime.Scheduler.current.wakeup_notification( self )
                self._wait.notify()

    def continuation_exception( self, exception ):
        with self._mutex:
            self._type = "exception"
            self._value = exception
            if self._state == "start":
                self._state = "signaled"
            else:
                self._state = "done"
                dramatis.runtime.Scheduler.current.wakeup_notification( self )
                self._wait.notify()

