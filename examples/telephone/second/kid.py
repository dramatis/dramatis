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
      return self.mangle( self._next and self._next.whisper( what ) or what )

  def mangle(self, what):
      return Mangler.mangle( what )
