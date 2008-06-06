from __future__ import absolute_import
from __future__ import with_statement

from logging import warning

import threading
from threading import Lock
from threading import Condition
from threading import Thread
from traceback import print_exc

import dramatis.runtime.actor

_checkio = False

_local = threading.local()
_local.dramatis_actor = None

# threading._VERBOSE = True

class ThreadPool(object):

    class __metaclass__(type):
        def __init__(self,*args,**kwds):
            # hmmmm ... ?
            super(self.__class__,self).__init__(*args,**kwds)
            self._mutex = Lock()
            self._threads = []
            self._state = "running"

        def checkout(self, target, args):
            assert self._state != "exiting"
            t = None
            with self._mutex:
                if len(self._threads) == 0:
                    pt = ThreadPool.PoolThread()
                    pt.setDaemon(True)
                    pt.start()
                    self._threads.append( pt )
                t = self._threads.pop()
            t._PoolThread__awaken( target, args )
            return t

        def checkin(self,thread):
            with self._mutex:
                if self._state != "exiting":
                    self._threads.append( thread )
                else:
                    warning("POST EXIT CHECKIN")

        def reset(self):
            self.exit()
            self._state = "running"

        def exit(self):
            with self._mutex:
                self._state = "exiting"
                for thread in self._threads:
                    thread._PoolThread__exit()
                for thread in self._threads:
                    thread.join()
                self._threads = []

        def __len__(self):
            with self._mutex: return len(self._threads)

    class PoolThread(Thread):

        def __init__(self):
            super(ThreadPool.PoolThread,self).__init__( target = self.__target )
            self.__mutex = Lock()
            self.__wait = Condition( self.__mutex )
            self.__state = "running"

        def __exit(self):
            with self.__mutex:
                old_state, self.__state = self.__state, "exiting"
                if old_state == "waiting":
                    self.__wait.notify()

        def __target( self ):
            while True:
                with self.__mutex:
                    # warning(str(self) + " llop start " + self.__state)
                    if self.__state == "exiting":
                        return
                    elif self.__state == "running":
                        self.__state = "waiting"
                        # warning(str(self) + " sleeping")
                        self.__wait.wait()
                        # warning(str(self) + " waking up")
                        assert self.__state != "waiting"
                    elif self.__state == "called":
                        try:
                            # warning(self.__current_target)
                            # warning(self.__args)
                            # warning(str(self) + " starting " + str(self.__args[0]))
                            self.__current_target(*self.__args)
                            # warning(str(self)+" done")
                        except:
                            print_exc()
                            raise
                        finally:
                            self.__state = "running"
                            ThreadPool.checkin( self )
                    else:
                        warning ( "!!FAIL!! " + self.__state)
                        raise "!!FAIL!! " + self.__state

        def __awaken(self,  target, args ):
            # warning( "awaken " + str(args[0]) )
            with self.__mutex:
                self.__current_target = target
                self.__args = args
                old_state, self.__state = self.__state, "called"
                if old_state == "waiting":
                    self.__wait.notify()
                elif old_state == "exiting":
                    warning("AWAKEN AFTER EXIT")
                    raise "AWAKEN AFTER EXIT"
                elif old_state != "running":
                    warning("AWAKEN BAD STATE " + old_state)
                    raise ("AWAKEN BAD STATE " + old_state)

    def __new__(cls, target, args ):
        # warning( "sched " + str(args[0]) )
        return cls.checkout(target, args)

class Scheduler(object):

    class __metaclass__(type):
        @property
        def current(self):
            if not hasattr(self,"_current"):
                self._current = self()
            return self._current

        def reset(self):
            ThreadPool.reset()
            self._current._reset()
            del self._current

        @property
        def actor(self):
            actor = None
            actor = _local.dramatis_actor
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

    def _reset(self): pass

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
        # warning('schedule ' + str(self._state) )
        with self._mutex:
            self._queue.append( task )
            if( len(self._queue) == 1 ):
                if( self._state == "waiting" ):
                    self._wait.notify()
                elif( self._state == "idle" ):
                    self._state = "running"
                    self._running_threads = 1
                    _checkio and warning( str(threading.currentThread()) + " checkout main; running will be " + str(self._running_threads) )
                    try:
                        t = Thread( target = self._run )
                        t.setDaemon(True)
                        t.start()
                    except Exception, e:
                        warning( "got an ex 0 " + repr(e) )
                        raise e

    class _Done(Exception):pass

    def _run(self):
        _checkio and warning( str(threading.currentThread()) + " scheduler starting " + str(self._state) )
        try:
            while True:
                with self._mutex:
                    while len(self._queue) == 0 and self._running_threads != 0:
                        self._state = "waiting"
                        try:
                            # warning( "schd sleeping " + str(threading.currentThread()) + " " + repr(self._wait))
                            self._wait.wait()
                            # warning( "schd awake " + str(threading.currentThread())  )
                        except Exception, exception:
                            # warning( "wait exception: #{exception}" )
                            pass
                        finally:
                            # warning( "schd running " + str(threading.currentThread())  )
                            self._state = "running"
                        
                try:
                    with self._mutex:
                        self._maybe_deadlock()
                except dramatis.Deadlock, deadlock:
                    actors = None
                    with self._mutex:
                        actors = list(self._actors)
                    for actor in actors:
                        _local.dramatis_actor = actor.name
                        actor.deadlock( deadlock )
                    _local.dramatis_actor = None

                with self._mutex:
                    self._maybe_deadlock()
    
                with self._mutex:
                
                    if( len(self._queue) == 0 and self._running_threads == 0 ):
                        raise Scheduler._Done()

                    if( len(self._queue) > 0 ):
                    
                        task = self._queue.pop(0)
            
                        self._running_threads += 1

                        try:
                            ThreadPool( target = self._deliver_thread,
                                        args = (task,) )
                        except Exception, e:
                            warning( "got an ex 1 " + repr(e) )
                            print_exc()
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
            print_exc()
            dramatis.Runtime.current.exception( exception )
    
        _checkio and warning( "scheduler giving up after final deadlock check #{self._queue.length} #{Thread.current}" )

        with self._main_mutex:
            state = self._main_state
            self._main_state = "may_finish"
            if( state == "waiting" ):
                self._main_join = threading.currentThread()
                try:
                    self._main_wait.notify()
                except Exception, e:
                    warning( "hell!!")
                    raise e

        if len(self._queue) > 0:
            raise "hell"
        self._state = "idle"
        self._thread = None

        _checkio and warning( "#{Thread.current} scheduler ending" )

    def _maybe_deadlock(self):
        # warning ( "maybe_deadlock " + str(threading.currentThread()) + " threads " + str(self._running_threads) + " q " + str(len(self._queue )) + " c " + str(len(self._suspended_continuations)) + " qi " + str(self._quiescing) )
        if( self._running_threads == 0 and len(self._queue) == 0 and
            len(self._suspended_continuations) > 0 and not self._quiescing ):
            # warning ( "DEADLOCK" )
            raise dramatis.Deadlock()

    def suspend_notification( self, continuation ):
        with self._mutex:
            if( self._state == "idle" ):
                self._state = "running"
                self._running_threads = 1
                _checkio and warning( "#{Thread.current} checkout--1 #{Thread.main} #{self._running_threads}" )
                try:
                    t = Thread( target = self._run )
                    t.setDaemon(True)
                    t.start()
                except Exception, e:
                    warning( "got an ex 2 " + repr(e) )
                    raise e
            _checkio and warning( str(threading.currentThread()) + " checkin-0; running will be " + str(self._running_threads-1) )
            self._running_threads -= 1
            if( self._state == "waiting" ):
                self._wait.notify()
            self._suspended_continuations[str(continuation)] = continuation

    def wakeup_notification( self, continuation):
        with self._mutex:
            del self._suspended_continuations[ str(continuation) ]
            self._running_threads += 1
            _checkio and warning( str(threading.currentThread()) + " checkout " + str(self._running_threads) )

    def quiesce(self):
        dramatis.runtime.actor.Main.current.quiesce()
        self._main_at_exit( True )

    def _main_at_exit( self,  quiescing = False ):
        # warning("main at exit " + str(quiescing) + " " + str(threading.currentThread()) )
        with self._mutex:
            self._quiescing = quiescing
            _checkio and warning( str(threading.currentThread()) + " main maybe checkin-1 " + str(self._running_threads) + " " +str(self._state) +" "+ str(self._main_state) +" " +str(quiescing) )
            if self._state != "idle":
                self._running_threads -= 1
                if self._state == "waiting":
                    try:
                        # warning( "notifying " + repr(self._wait) )
                        # warning( str(threading.enumerate()) )
                        self._wait.notify()
                        # warning( str(threading.enumerate()) )
                        # warning( "notified" + repr(self._wait)  )
                    except Exception, e:
                        warning( "crap " + str(e) )
                        raise e;

            _checkio and warning( str(threading.currentThread() ) + " main signaled " + str(self._running_threads) + " " + str(self._state) + " " + str(self._main_state) + " " + str(quiescing) )

        with self._main_mutex:
            if self._main_state == "running":
                with self._mutex:
                    if self._state != "idle":
                        self._main_state = "waiting"
                if self._main_state == "waiting":
                    # warning( "main waiting " + str(threading.currentThread()) )
                    self._main_wait.wait()
                    # warning( "main finished waiting " + str(threading.currentThread()) )
                    self._main_join.join()
                    self._main_join = None
            else:
                self._maybe_deadlock()
                self._main_state = "may_finish"

            if self._quiescing:
                self._main_state = "running"
            self._quiescing = False

        ThreadPool.reset()

        dramatis.Runtime.current._maybe_raise_exceptions( quiescing )

    def _deliver_thread(self,*args):
        _checkio and warning( str(threading.currentThread()) + " spining up; running will be " + str(self._running_threads) )
        try:
            self.deliver( args[0] )
        except Exception, e:
            warning( "unexptected deliver error " + repr(e) )
            raise e
        finally:
            with self._mutex:
                self._running_threads -= 1
                _checkio and warning( str(threading.currentThread()) + " checkin-2 / retiring; now running " + str(self._running_threads) + " " + str(self._state) )
                # warning( threading.enumerate() )
                if( self._state == "waiting" ):
                    self._wait.notify()

    def deliver( self, task ):
        _local.dramatis_actor = task.actor.name
        try:
            task.deliver()
        except Exception, exception:
            warning( "3 exception " + str(exception) )
            print_exc()
            dramatis.Runtime.current.exception( exception )
        finally:
            _local.dramatis_actor = None

    ThreadPool = ThreadPool
