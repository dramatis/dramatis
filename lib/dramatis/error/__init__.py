from __future__ import absolute_import

class Error(Exception): pass

class Interface(Error): pass

class Uncaught(Error): pass

class Bind(Error): pass

class Internal(Error): pass
