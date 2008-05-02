from __future__ import absolute_import

import dramatis

class Interface(object):

    def __init__(self,name):
        self._name = name

    def continuation( self, options = {} ):
        a = super(dramatis.Actor.Name,self._name).__getattribute__("_actor")
        o = super(dramatis.Actor.Name,self._name).__getattribute__("_options")
        name = self._name = dramatis.Actor.Name(a)
        new_options = o.copy()
        new_options["continuation"] = "none" if options == None else "continuation"
        if( options ):
            new_options["exception"] = options["exception"]
        super(dramatis.Actor.Name,name).__setattr__("_options",new_options)
        return name


    def exception( self, exception ):
        return self._actor_send( "exception", exception )

    def _actor_send( self, *args ):
        a = super(dramatis.Actor.Name,self._name).__getattribute__("_actor")
        o = super(dramatis.Actor.Name,self._name).__getattribute__("_options")
        return a.actor_send( args, o )

    def _continuation(self, c, options):
        a = super(dramatis.Actor.Name,self._name).__getattribute__("_actor")
        o = super(dramatis.Actor.Name,self._name).__getattribute__("_options")

        a.register_continuation( c )

        name = self._name = dramatis.Actor.Name(a)

        new_options = o.copy()
        new_options["continuation"] = "none"
        new_options["continuation_send"] = str(c)
        ct = options.get("call_thread")
        if( ct ):
            new_options["call_thread"] = ct

        super(dramatis.Actor.Name,name).__setattr__("_options",new_options)

        return name

    '''
  # call-seq:
  #   future -> a_name
  # 
  # Returns a new actor name that when used in an actor method call will return a Dramatis::Future. Usually
  # called via Dramatis.future rather than directly.

  def future
    a, o = self._name.instance_eval { [ self._actor, self._options ] }
    self._name = Dramatis::Actor::Name.new a
    self._name.instance_eval do
      self._options = o.dup
      self._options[:continuation] = :future
    end
    self._name
  end

  # Binds the actor identified by this name to supplied behavior,
  # which should be a native ruby object.  Can only be called on
  # unbound actors, typically created with Dramatis::Actor.new().  The
  # result of the call is the actor name of the actor. The
  # continuation semantics of the call depend on the name like a
  # normal actor method call.

  def bind behavior
    actor_send :bind, behavior
  end

  def url #:nodoc: not done
    "http://something"
  end

  private

  def continuation c, options
    a, o = self._name.instance_eval { [ self._actor, self._options ] }
    self._name = Dramatis::Actor::Name.new a
    self._name.instance_eval do
      self._actor.register_continuation c
      self._options = o.dup
      self._options[:continuation_send] = c.to_s
      self._options[:continuation] = :none
      # FIX merge options, rather than cherry-pck
      options[:call_thread] and self._options[:call_thread] = options[:call_thread]
    end
    self._name
  end

'''


