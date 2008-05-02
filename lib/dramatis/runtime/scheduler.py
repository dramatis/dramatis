from __future__ import absolute_import
from __future__ import with_statement

from logging import warning

import threading
from threading import Lock
from threading import Condition
from threading import Thread

import dramatis.runtime.actor

_checkio = True

class Scheduler(object):

    class __metaclass__(type):
        @property
        def current(self):
            if not hasattr(self,"_current"):
                self._current = self()
            return self._current

        @property
        def actor(self):
            actor = None
            try:
                actor = threading.local().dramatis_actor
            except AttributeError: pass
            isMain = threading.currentThread().getName() == "MainThread"
            if( not actor ):
                if( isMain ):
                    actor = dramatis.runtime.actor.Main.current.name
            else:
                if( isMain and
                    actor != dramatis.runtime.actor.Main.current ):
                    raise "hell"
                if( not isMain and
                    actor == dramatis.runtime.actor.Main.current ):
                    raise "hell"
            return actor

    def __init__(self):
        self._mutex = Lock()
        self._wait = Condition(self._mutex)
        self._running_threads = 0
        self._suspended_continuations = {}
        self._queue = []
        self._state = "idle"

        self._main_mutex = Lock()
        self._main_wait = Condition(self._main_mutex)
        self._main_state = "running"
        self._quiescing = False

        self._thread = None

        self._actors = []

    def append(self,actor):
        self._actors.append( actor )


    def schedule( self, task ):
        with self._mutex:
            self._queue.append( task )
            if( len(self._queue) == 1 ):
                if( self._state == "waiting" ):
                    self._wait.notify()
                elif( self._state == "idle" ):
                    self._state = "running"
                    self._running_threads = 1
                    _checkio and warning( "%s checkout main %d" % ( threading.currentThread(), self._running_threads ) )
                    try:
                        Thread( target = self._run ).start()
                    except Exception, e:
                        warning( "got an ex 0 " + repr(e) )
                        raise e

    class _Done(Exception):pass

    def _run(self):
        _checkio and warning( "%s scheduler starting %s" % ( threading.currentThread(), self._state ) )
        try:
            while True:
                with self._mutex:
                    while len(self._queue) == 0 and self._running_threads != 0:
                        self._state = "waiting"
                        try:
                            self._wait.wait()
                        except Exception, exception:
                            warning( "wait exception: #{exception}" )
                        finally:
                            self._state = "running"
                        
                try:
                    with self._mutex:
                        self._maybe_deadlock()
                except dramatis.Deadlock, deadlock:
                    actors = None
                    with self._mutex:
                        actors = list(self._actors)
                        local = threading.local()
                        for actor in actors:
                            local.dramatis_actor = actor.name
                            actor.deadlock( deadlock )
                        local.dramatis_actor = None

                with self._mutex:
                    self._maybe_deadlock()
    
                with self._mutex:
                
                    if( len(self._queue) == 0 and self._running_threads == 0 ):
                        raise Scheduler._Done()

                if( len(self._queue) > 0 ):
                    
                    task = self._queue.pop(0)
            
                    self._running_threads += 1

                    try:
                        Thread( target = self._deliver_thread, args = (task,) ).start()
                    except Exception, e:
                        warning( "got an ex 1 " + repr(e) )
                        raise e

        except Scheduler._Done: pass
        except Exception, exception:
            warning( "1 *? exception " + str(exception) )
            dramatis.Runtime.current.exception( exception )

        _checkio and warning( "scheduler giving up the ghost #{self._queue.length} #{Thread.current}" )

        try:
            with self._mutex:
                self._maybe_deadlock()
        except dramatis.Deadlock, deadlock:
            actors = []
            with self._mutex:
                actors = list(self._actors)
            for actor in actors:
                actor.deadlock( deadlock )
        except Exception, exception:
            warning( "2 exception " + str(exception) )
            dramatis.Runtime.current.exception( exception )
    
        _checkio and warning( "scheduler giving up after final deadlock check #{self._queue.length} #{Thread.current}" )

        with self._main_mutex:
            state = self._main_state
            self._main_state = "may_finish"
            if( state == "waiting" ):
                self._main_join = threading.currentThread()
                self._main_wait.notify()

        if len(self._queue) > 0:
            raise "hell"
        self._state = "idle"
        self._thread = None

        _checkio and warning( "#{Thread.current} scheduler ending" )

    def _maybe_deadlock(self):
        if( self._running_threads == 0 and len(self._queue) == 0 and
            len(self._suspended_continuations) > 0 and not self._quiescing ):
            raise dramatis.Deadlock()

    def suspend_notification( self, continuation ):
        with self._mutex:
            if( self._state == "idle" ):
                self._state = "running"
                self._running_threads = 1
                _checkio and warning( "#{Thread.current} checkout -1 #{Thread.main} #{self._running_threads}" )
                try:
                    Thread( target = self._run ).start()
                except Exception, e:
                    warning( "got an ex 2 " + repr(e) )
                    raise e
            _checkio and warning( "%s checkin 0 %d" % ( threading.currentThread(), self._running_threads ) )
            self._running_threads -= 1
            if( self._state == "waiting" ):
                self._wait.notify()
            self._suspended_continuations[str(continuation)] = continuation

    def _deliver_thread(self,*args):
        _checkio and warning( "#{Thread.current} spining up #{self._running_threads}" )
        try:
            self.deliver( args[0] )
        except Exception, e:
            warning( "unexptected deliver error " + repr(e) )
            raise e
        finally:
            with self._mutex:
                _checkio and warning( "#{Thread.current} checkin 2 #{self._running_threads} #{self._state}" )
                self._running_threads -= 1
                if( self._state == "waiting" ):
                    self._wait.notify()

    def deliver( self, task ):
        local = threading.local()
        local.dramatis_actor = task.actor.name
        try:
            task.deliver()
        except Exception, exception:
            warning( "2 exception " + str(exception) )
            dramatis.Runtime.current.exception( exception )
        finally:
            local.dramatis_actor = None

