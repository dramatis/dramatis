from __future__ import absolute_import
from __future__ import with_statement

from logging import warning

import threading
from threading import Lock
from threading import Condition
from threading import Thread
from traceback import print_exc

class ThreadPool(object):

    def reset(self, soft = False):
        self._shutdown( soft )
        self._state = "running"

    def __init__(self):
        super(ThreadPool,self).__init__()
        self._mutex = Lock()
        self._wait = Condition(self._mutex)
        self._threads = []
        self._size = 0
        self._state = "running"

    def __call__(self, target, args = []):
        return self.checkout(target, args)

    @property
    def length(self):
        with self._mutex:
            return len(self._threads)

    @property
    def size(self):
        with self._mutex:
            return self._size

    def _shutdown(self,soft):
        with self._mutex:
            self._state = "exiting"
        first = True
        with self._mutex:
            while first or ( not soft and self._size > 0 ):
                first = False
                for thread in self._threads:
                    thread._PoolThread__shutdown()
                while len(self._threads) > 0:
                    thread = self._threads.pop()
                    thread.join()
                    self._size -= 1
                if not soft and self._size > 0:
                    self._state = "exit_waiting"
                    self._wait.wait()
                    self._state = "exiting"

    def checkin(self,thread):
        with self._mutex:
            self._threads.append( thread )
            if self._state == "exit_waiting":
                self._wait.notify()

    def checkout(self, target, args):
        assert self._state != "exiting"
        t = None
        with self._mutex:
            if len(self._threads) == 0:
                pt = PoolThread(self)
                pt.setDaemon(True)
                pt.start()
                self._threads.append( pt )
                self._size += 1
            t = self._threads.pop()
        t._PoolThread__awaken( target, args )
        return t

class PoolThread(Thread):

    def __init__(self,pool):
        super(PoolThread,self).__init__( target = self.__target )
        self._pool = pool
        self.__mutex = Lock()
        self.__wait = Condition( self.__mutex )
        self.__state = "running"

    def __shutdown(self):
        with self.__mutex:
            old_state, self.__state = self.__state, "exiting"
            if old_state == "waiting":
                self.__wait.notify()

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
                        self._pool.checkin( self )
                else:
                    warning ( "!!FAIL!! " + self.__state)
                    raise "!!FAIL!! " + self.__state


