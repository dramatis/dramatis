import time

import dramatis

from  mangler import Mangler

class Kid( dramatis.Actor ):
  
  def __init__(self, name, next_kid = None):
      super(Kid,self).__init__()
      self._name = name
      self._next = next_kid
      self._heard = "I ain't hear nuthin"
      self.actor.refuse( "ask" )

  def __str__(self):
      return self._name
  
  def mangle(self, what):
      return Mangler.mangle( what )

  def whisper(self, what):
      self._heard = self.mangle( what )
      # time.sleep(1)
      # print str(self)+" heard "+str(self._heard)
      if self._next:
          dramatis.release( self._next ).whisper( self._heard )
      self.actor.accept( "ask" )
      self.actor.refuse( "whisper" )

  def ask(self):
      self.actor.refuse( "ask" )
      self.actor.accept( "whisper" )
      return self._heard

