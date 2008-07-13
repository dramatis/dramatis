module Dramatis; end
module Dramatis::Actor; end
class Dramatis::Actor::Name; end

# A Dramatis::Actor::Name::Interface object provides the ability to
# modify the semantics of actor name and perform other actor-level operations on an
# actor. It is typically created via Dramatis.interface.

class Dramatis::Actor::Name::Interface

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

  def continue options = {}, &continuation
    raise "contradictory options passed to continue" \
        if ( options == nil and continuation ) or
           ( options and !options[:continuation] and !continuation )
    a, o = @name.instance_eval { [ @actor, @options ] }
    @name = Dramatis::Actor::Name.new a
    @name.instance_eval do
      @options = o.dup
      @options[:continuation] = options == nil ? :none : ( options[:continuation] or continuation )
      options and @options[:exception] = options[:exception]
      options and @options[:nonblocking] = options[:nonblocking]
    end
    @name
  end

  # call-seq:
  #   future -> a_name
  # 
  # Returns a new actor name that when used in an actor method call will return a Dramatis::Future. Usually
  # called via Dramatis.future rather than directly.

  def future
    a, o = @name.instance_eval { [ @actor, @options ] }
    @name = Dramatis::Actor::Name.new a
    @name.instance_eval do
      @options = o.dup
      @options[:continuation] = :future
    end
    @name
  end

  # call-seq:
  #   bind( behavior ) -> actor_name
  # 
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
    a, o = @name.instance_eval { [ @actor, @options ] }
    @name = Dramatis::Actor::Name.new a
    @name.instance_eval do
      @actor.register_continuation c
      @options = o.dup
      @options[:continuation_send] = c.to_s
      @options[:continuation] = :none
      # FIX merge options, rather than cherry-pck
      options[:call_thread] and @options[:call_thread] = options[:call_thread]
    end
    @name
  end

  def actor_send *args, &block
    @name.instance_eval do
      options = @options
      if block
        options = options.dup
        options[:block] = block
      end
      @actor.actor_send args, options
    end
  end

  def initialize name #:nodoc:
    raise "hell: " + name.inspect \
      if !name or !name.kind_of? Dramatis::Actor::Name
    @name = name
  end

end

