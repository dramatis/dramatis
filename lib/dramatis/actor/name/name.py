import logging

class Proxy(object):
    def __init__(self,actor,name):
        self.actor = actor
        self.name = name

    def __call__(self,*args,**kwds):
        # logging.warning([args,kwds])
        return self.actor.behavior.__getattribute__(self.name).__call__(*args,**kwds)

class Name(object):

    def __init__(self,actor):
        super(Name,self).__setattr__("actor",actor)

    def __getattribute__(self,name):
        # logging.warning(Proxy)
        return Proxy(super(Name,self).__getattribute__("actor"),name)

