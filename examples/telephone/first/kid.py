import dramatis

from  mangler import Mangler

class Kid( dramatis.Actor ):
  
  def __init__(self, name, next_kid = None):
      super(Kid,self).__init__()
      self._name = name
      self._next = next_kid

  def __str__(self):
      return self._name
  
  def whisper(self, what):
      what = self.mangle( what )
      if self._next:
          self._next.whisper( what )
      else:
          print "%s: %s" % ( self, what )

  def mangle(self, what):
      return Mangler.mangle( what )
