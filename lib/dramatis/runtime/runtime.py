from __future__ import absolute_import
from __future__ import with_statement

from logging import warning

from threading import Lock

import dramatis.runtime
import dramatis.runtime.actor

class Runtime:
    """Top level class managing the running of the various pieces of the dramatis runtime.

    Typically programs don't need to deal with the runtime directly,
    though some functions are useful, particularly for debugging and
    testing."""

    class __metaclass__(type):
        @property
        def current(self):
            """Returns a reference to the current Dramatis::Runtime object."""

            if not hasattr(self,"_current"):
                self._current = self()
            return self._current

        def reset(self):
            """Resets the current runtime instance.

            Note that this method hard resets counters and ignores
            exceptions which is generally a bad idea. It is typical
            only used in unit test and spec "after" methods to keep
            failing tests from cascading."""

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
        """Causes the runtime to suspend the current thread until
        there are no more tasks that can be executed.

        If no tasks remain, returns normally. If tasks remain but are
        gated off, dramatis.Deadlock is raised.

        As a side effect, this method releases the current actor to
        process messages but does not change the task gate."""
  
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
        """Returns the list of exceptions that were not caught by an actor."""

        result = []
        with self._mutex:
            result = list(self._exceptions)
        return result

    def clear_exceptions(self):
        """Clears the list of uncaught exceptions.

        Used in unit tests and specs to clear expected exceptions.  If
        exceptions are raised and not cleared, they will be raised at
        the end of the program via a dramatis.error.Uncaught."""

        with self._mutex:
            # warn "runtime clearing exceptions"
            self._exceptions[:] = []

    def exception( self, exception ):
        print dramatis.error.tracehook( exception )
        with self._mutex:
            self._exceptions.append( exception )


    def at_exit( self ):
        dramatis.runtime.actor.Main.current.finalize()

