from __future__ import with_statement

import atexit

from dramatis.runtime.actor import Actor

class Main ( Actor ):

  class __metaclass__(type):
      @property
      def current(self):
          if not hasattr(self,"_current"):
              self._current = self()
          return self._current

      def reset(self):
          self._current = None

  def quiesce(self):
      self.schedule()

  def finalize(self):
      if( not self._at_exit_run ):
          self._at_exit_run = True
      self.schedule()
      Scheduler.current.main_at_exit()

  def __init__(self):
      super(Main,self).__init__( Main.DefaultBehavior() )
      self._at_exit_run = False
      atexit.register( self.finalize )
      self.make_runnable()

  class DefaultBehavior(object): pass

