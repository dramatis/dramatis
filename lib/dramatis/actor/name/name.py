import logging

class Proxy(object):
    def __init__(self,actor,name):
        super(Proxy,self).__setattr__("actor",actor)
        super(Proxy,self).__setattr__("name",name)
        super(Proxy,self).__setattr__("options",{"continuation":"rpc"})

    def __call__(self,*args,**kwds):
        # logging.warning([args,kwds])
        actor = super(Proxy,self).__getattribute__("actor")
        name = super(Proxy,self).__getattribute__("name")
        options = super(Proxy,self).__getattribute__("options")
        return actor.object_send( name, args, kwds, options )

class Name(object):

    def __init__(self,actor):
        super(Name,self).__setattr__("actor",actor)

    def __getattribute__(self,name):
        # logging.warning(Proxy)
        return Proxy(super(Name,self).__getattribute__("actor"),name)

