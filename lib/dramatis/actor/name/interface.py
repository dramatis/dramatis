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
        new_options["continuation"] = "none" if args[0] == None else "continuation"
        if( options ):
            new_options["exception"] = options["exception"]
        super(dramatis.Actor.Name,name).__setattr__("_options",new_options)
        return name


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
  #  continue nil -> a_name
  #  continue { |retval| ... } -> a_name
  #  continue( :exception => lambda { |exception| ... } ) { |retval| ... } -> a_name
  #
  # In call cases, returns a new name with the specified continuation semantics.
  #
  # When passed a nil argument, returns a new actor name with a nil
  # continuation such that when used in an actor method call, the call
  # will return nil immediately. The return value from such a call is
  # lost. Equivalent to and usually called as Dramatis.release.
  #
  # The second form sets up the block passed to the function as the
  # continuation of the call. When the continuation task is received from the
  # target actor, the block will be executed. From senders point of
  # view, the block is an unnamed method: it will only be
  # scheduled when the actor is not executing any other task.
  #
  # Currently it is not possible to gate off block continuations.
  #
  # The third example is a variant on the second and is used to
  # provide a second block to receive an exception object if the actor
  # method call results in an exception being thrown. Otherwise, the
  # runtime will try to deliver exceptions to a dramatis_exception
  # actor method if defined. Otherwise it will be recored by the
  # runtime.
  #
  #--
  # this stuff is either tricky or evil; i need to lookup
  # variable look ordering for instance_eval
  # i'm assuming lexical scope over object scope
  #++

  
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

  def exception exception #:nodoc: this should be private/protected
    actor_send :exception, exception
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

  def actor_send *args, &block
    self._name.instance_eval do
      options = self._options
      if block
        options = options.dup
        options[:block] = block
      end
      self._actor.actor_send args, options
    end
  end

  def initialize name #:nodoc:
    raise "hell: " + name.inspect \
      if !name or !name.kind_of? Dramatis::Actor::Name
    self._name = name
  end

end
'''


