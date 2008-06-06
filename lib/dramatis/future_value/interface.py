from __future__ import absolute_import

from logging import warning

import dramatis

class Interface(object):
    """Inteface to observe and access the semantics of a future.

    It is typically created via Dramatis.interface."""

    def __init__(self,future):
        self._future = future

    @property
    def ready(self):
        """Returns true if the future may be evaluated without blocking.

        Returns false if the value is not yet available.

        Once a future is ready it cannot become unready, so once
        ready? returns true, it will always be true and value access
        will never block."""

        return super(dramatis.Future,self._future).__getattribute__("_continuation").ready

    @property
    def value(self):
        """Returns the native value of the future.

        If the value of the future is not yet available, the method
        blocks (with rpc gating semantics) until it is.

        In many cases, this method is not necessary since the
        method_missing method on the future will catch most attempts
        to accesses the value. This method may be necessary in corner
        cases, for example when using conditionals, conversions, and
        metaprogramming."""

        return super(dramatis.Future,self._future).__getattribute__("_continuation").value

