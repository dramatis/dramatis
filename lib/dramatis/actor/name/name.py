from __future__ import absolute_import

import logging

from dramatis.actor.name.interface import Interface as _Interface

class Proxy(object):
    def __init__(self,attr,actor,options):
        super(Proxy,self).__setattr__("_attr",attr)
        super(Proxy,self).__setattr__("_actor",actor)
        super(Proxy,self).__setattr__("_options",options)

    def __call__(self,*args,**kwds):
        actor = super(Proxy,self).__getattribute__("_actor")
        attr = super(Proxy,self).__getattribute__("_attr")
        options = super(Proxy,self).__getattribute__("_options")
        return actor.object_send( attr, args, kwds, options )

class Name(object):

    def __init__(self,actor):
        super(Name,self).__setattr__("_actor",actor)
        super(Name,self).__setattr__("_options",{"continuation":"rpc"})

    def __getattribute__(self,attr):
        # logging.warning(Proxy)
        a = super(Name,self).__getattribute__("_actor")
        o = super(Name,self).__getattribute__("_options")
        return Proxy(attr,a,o)

    Interface = _Interface
