from __future__ import absolute_import
from __future__ import with_statement

from logging import warning

from threading import Lock

class Runtime:

    class __metaclass__(type):
        @property
        def current(self):
            if not hasattr(self,"_current"):
                self._current = self()
            return self._current

    @classmethod
    def _reset(self):
        pass

    def __init__(self):
        self._warnings = True
        self._mutex = Lock()
        self._exceptions = []

    def _quiesce(self):
        pass

    def _exceptions(self):
        return ()

    def exception( self, exception ):
        with self._mutex:
            self._exceptions.append( exception )

