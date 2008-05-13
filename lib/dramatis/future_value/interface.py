from __future__ import absolute_import

from logging import warning

import dramatis

class Interface(object):
    def __init__(self,future):
        self._future = future
    @property
    def ready(self):
        return super(dramatis.Future,self._future).__getattribute__("_continuation").ready
    @property
    def value(self):
        return super(dramatis.Future,self._future).__getattribute__("_continuation").value

