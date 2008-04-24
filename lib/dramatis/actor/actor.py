from __future__ import absolute_import

from dramatis.actor.name import Name as _Name

class Metaclass(type):

    def __init__(cls):
        assert 0

class Actor(object):

    __Metaclass__ = Metaclass

    Name = _Name

