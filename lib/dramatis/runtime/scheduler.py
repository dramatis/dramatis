from __future__ import absolute_import
from __future__ import with_statement

from threading import Lock
from threading import Condition

class class_property(property):
    def __get__(self,inst,own):
        print "get", self, inst, own
        if inst:
            return super(class_property,self).__get__(inst,own)
        else:
            return super(class_property,self).__get__(own,own)

    def __set__(self,inst,v):
        print "set", self, inst, v
        if inst:
            return super(class_property,self).__set__(inst,v)
        else:
            return super(class_property,self).__set__(inst,v)

    def __delete__(self,inst):
        print "del", self, inst
        if inst:
            return super(class_property,self).__delete__(inst)
        else:
            return super(class_property,self).__delete__(inst)

class Scheduler(object):

    class __metaclass__(type):
        @property
        def current(self):
            if not hasattr(self,"_current"):
                self._current = self()
            return self._current

    def __init__(self):
        self.mutex = Lock()
        self.wait = Condition(self.mutex)
        self.running_threads = 0
        self.suspended_continuations = {}
        self.queue = []
        self.state = "idle"

        self.main_mutex = Lock()
        self.main_wait = Condition(self.main_mutex)
        self.main_state = "running"
        self.quiescing = False

        self.thread = None

        self.actors = []

    def append(self,actor):
        self.actors.append( actor )
