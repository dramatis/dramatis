from __future__ import with_statement

from logging import warning
import atexit
import sys


import dramatis.runtime
from dramatis.runtime.actor import Actor

def _excepthook( type, value, traceback ):
  tb = dramatis.error.traceback( value )
  tb.set(traceback)
  print "Traceback (most recent call last):"
  print str(tb),
  print "%s: %s" % ( type.__name__, value )
  # return sys.__excepthook__( type, value, tb.traceback )
  # return sys.__excepthook__( type, value, traceback )

class Main ( Actor ):

  class __metaclass__(type):
      @property
      def current(self):
          if not hasattr(self,"_current"):
              self._current = self()
          return self._current

      def reset(self):
          del self._current

  def quiesce(self):
      self.schedule()

  def finalize(self):
      if( not self._at_exit_run ):
          self._at_exit_run = True
      self.schedule()
      dramatis.runtime.Scheduler.current._main_at_exit()
      sys.excepthook = sys.__excepthook__

  def __init__(self):
      super(Main,self).__init__( Main.DefaultBehavior() )
      self._at_exit_run = False
      atexit.register( self.finalize )

      sys.excepthook = _excepthook

      self.make_runnable()

  class DefaultBehavior(object): pass

