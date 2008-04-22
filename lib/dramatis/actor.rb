module Dramatis; end
module Dramatis::Actor; end

require 'dramatis/runtime/actor'

# The Dramatis::Actor module is used to create actor classes and
# objects. An actor class can be created by mixing Dramatis::Actor, e.g.,
#    class MyClass
#      include Dramatis::Actor
#      ...
#    end
# or can be used to create so called _naked_ _actors_, e.g.,
#    my_hash_actor = Dramatis::Actor.new Hash.new
#
# When mixed in to a class, Dramatis::Actor has two effects:
# 1. It causes new to return a Dramatis::Actor::Name rather than an object reference
# 1. It defines an actor method which can be used by the class to access its actor name and
#    otherwise affect its actor semantics

module Dramatis::Actor

  def self.included cls #:nodoc:
    cls.instance_eval do
      include Dramatis
    end
    class << cls
      def new *args
        new_actor = Dramatis::Runtime::Actor.new
        object = allocate
        eigenclass = ( class << object; self; end )
        eigenclass.send :define_method, :actor,
                ( lambda { new_actor.object_interface } )
        new_actor.bind object
        new_actor.instance_eval { @gate.refuse :object }
        new_actor.actor_send [ :object_initialize, *args ], 
                               :continuation => :rpc
        new_actor.name
      end
    end
  end

  if false # for docs only ...

    # call-seq:
    #  actor -> an_interface
    #
    # actor provides classes that have mixed in Dramatis::Actor access
    # to a Dramatis::Actor::Interface object by which they can access
    # their actor name and other actor operations.

    def actor; end

  end

  # call-seq:
  #  new( *args, &block ) -> an_actor_name
  #  new( behavior = nil ) -> an_actor_name
  #
  # The first case is used when a class has mixed in
  # Dramatis::Actor. In this case, the arguments are passed to the 
  # initialize of method of the including class like normal.
  #
  # The second case is used when creating so called <em>naked
  # actors</em>, e.g.,
  #  my_hash = Dramatis::Actor.new Hash.new
  # If no
  # behavior is provided, the actor can be later bound to a behavior
  # by calling Dramatis::Actor::Name::Interface.bind
  #
  # In all cases, new returns a Dramatis::Actor::Name proxy
  # object.

  def self.new behavior = nil
    ( Dramatis::Runtime::Actor.new behavior ).name
  end

  if false
    def self.included cls #:nodoc:
      pp caller(0)
      warn "Dramatis::Actor included by #{cls}"
    end

    def self.derived cls #:nodoc:
      warn "Dramatis::Actor included by #{cls}"
    end

    def self.extended cls #:nodoc:
      warn "Dramatis::Actor included by #{cls}"
    end

    def self.inherited cls #:nodoc:
      warn "Dramatis::Actor included by #{cls}"
    end
  end

end
