from __future__ import absolute_import
from __future__ import with_statement

from logging import warning

from threading import Lock

import dramatis.runtime
import dramatis.runtime.actor

class Runtime:

    class __metaclass__(type):
        @property
        def current(self):
            if not hasattr(self,"_current"):
                self._current = self()
            return self._current

        def reset(self):
            try:
                Runtime.current.quiesce()
            except Exception, e: pass
            dramatis.runtime.Scheduler.reset()
            dramatis.runtime.actor.Main.reset()
            del self._current

    def __init__(self):
        self.warnings = True
        self._mutex = Lock()
        self._exceptions = []

    def quiesce(self):
        dramatis.runtime.Scheduler.current.quiesce()    
        self._maybe_raise_exceptions( True )

    def _maybe_raise_exceptions( self, quiescing):
        with self._mutex:
            if len(self._exceptions) > 0:
                if not quiescing and self.warnings:
                    warning( "the following #{@exceptions.length} exception(s) were raised and not caught" )
                    for exception in self._exceptions:
                        warning( "#{exception}" )
                        # pp exception.backtrace

                raise dramatis.error.Uncaught( self._exceptions )

            self._exceptions[:] = []

    def exceptions(self):
        result = []
        with self._mutex:
            result = list(self._exceptions)
        return result

    def clear_exceptions(self):
        with self._mutex:
            # warn "runtime clearing exceptions"
            self._exceptions[:] = []

    def exception( self, exception ):
        print dramatis.error.tracehook( exception )
        with self._mutex:
            self._exceptions.append( exception )


    def at_exit( self ):
        dramatis.runtime.actor.Main.current.finalize()

