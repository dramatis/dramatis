module Dramatis; end
module Dramatis::Actor; end

require 'dramatis/actor/name/proxy'
require 'dramatis/actor/name'

module Dramatis::Actor

  Runtime = Dramatis::Runtime
  Proxy = Dramatis::Actor::Name::Proxy

  def self.Name *args, &block
    Proxy.new( *args, &block )
  end

  def self.acts_as cls, opts = {}

    if opts[:new] != :object
      cls.class_eval do
        def self.new *args
          new_actor = Runtime::Actor.new
          object = allocate
          ( class << object; self; end ).send :define_method, :actor,
                                ( lambda { new_actor.object_interface } )
          new_actor.bind object
          new_actor.instance_eval { @gate.set_default false, :object }
          new_actor.actor_send [ :object_initialize, *args ], :continuation => :rpc
          new_actor.name
        end
      end
    end

  end

  def self.new behavior = nil
    ( Runtime::Actor.new behavior ).name
  end

  def self.current
    Dramatis::Runtime::Scheduler.current
  end

end
