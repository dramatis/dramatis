from __future__ import absolute_import

import dramatis

def _func(): pass
_func = type(_func)

class Interface(object):
    """Access to additional dyanmics of actor names.

    A dramatis.Ator.Name.Interface object provides the ability to
    modify the semantics of actor name and perform other actor-level
    operations on an actor. It is typically created via
    dramatis.interface."""

    def __init__(self,name):
        self._name = name


    def continuation( self, options = {} ):
        """return a new name with the specified continuation semantics.
        
        continue( None ) -> a_name
        continue( "result": function,
                  "exception": function ) -> a_name

        When passed a None argument, returns a new actor name with a
        None continuation such that when used in an actor method call,
        the call will return None immediately. The return value from
        such a call is lost. Equivalent to and usually called as
        dramatis.release.
        
        The second form sets up the function objects passed as
        the continuation of the call. When the continuation task is
        received from the target actor, the function will be
        executed. From senders point of view, the block is an unnamed
        method: it will only be scheduled when the actor is not
        executing any other task.
        
        Currently it is not possible to gate off block continuations.
        
        If an exception method is given, it will be called if the actor
        method call results in an exception being thrown. Otherwise, the
        runtime will try to deliver exceptions to a dramatis_exception
        actor method if defined. Otherwise it will be recored by the
        runtime."""
        
        a = super(dramatis.Actor.Name,self._name).__getattribute__("_actor")
        o = super(dramatis.Actor.Name,self._name).__getattribute__("_options")
        name = self._name = dramatis.Actor.Name(a)
        new_options = o.copy()
        new_options["continuation"] = "none"
        if ( options ):
            if type(options) == _func:
                new_options["continuation"] = options
            else:
                if options.has_key( "result" ):
                    new_options["continuation"] = options["result"]
                if options.has_key( "exception" ):
                    new_options["exception"] = options["exception"]
        if new_options["continuation"] == None:
            new_options["continuation"] = "none"
        super(dramatis.Actor.Name,name).__setattr__("_options",new_options)
        return name

    def future(self):
        """Returns a new actor name that when used in an actor method call will return a dramatis.Future.
        
        Usually called via dramatis.future rather than directly."""

        a = super(dramatis.Actor.Name,self._name).__getattribute__("_actor")
        o = super(dramatis.Actor.Name,self._name).__getattribute__("_options")
        self._name = dramatis.Actor.Name(a)
        new_options = o.copy()
        new_options["continuation"] = "future"
        super(dramatis.Actor.Name,self._name).__setattr__("_options",new_options)
        return self._name

    def exception( self, exception ):
        return self._actor_send( "exception", exception )

    def bind(self, behavior):
        """Binds the actor identified by this name to supplied behavior.
        
        The behavior shoudl be a native object.  Can only be called on
        unbound actors, typically created with dramatis.Actor().  The
        result of the call is the actor name of the actor. The
        continuation semantics of the call depend on the name like a
        normal actor method call."""

        return self._actor_send( "bind", behavior )

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


