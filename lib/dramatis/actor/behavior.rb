module Dramatis; end
module Dramatis::Actor; end

require 'dramatis/actor'

# The Dramatis::Actor::Behavior module is used to create actor behaviors.
# Creating an actor behavior does not create an actor but creates an object
# that an actor can "become" while still providing the actor method hook
# for value-add operations.

module Dramatis::Actor::Behavior

  def self.included cls #:nodoc:
    cls.instance_eval do
      include Dramatis
    end
    class << cls
      def new *args
        object = allocate
        eigenclass = ( class << object; self; end )
        eigenclass.send :define_method, :actor,
                ( lambda { Dramatis::Actor::Interface.new nil } )
        object.send :initialize, *args
        object
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

end


