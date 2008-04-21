module Dramatis; end
module Dramatis::Actor; end

require 'dramatis/runtime/actor'

module Dramatis::Actor

  def self.included cls
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

  def self.new behavior = nil
    ( Dramatis::Runtime::Actor.new behavior ).name
  end

  def self.current
    Dramatis::Runtime::Scheduler.actor
  end

  if false
    def self.included cls
      pp caller(0)
      warn "Dramatis::Actor included by #{cls}"
    end

    def self.derived cls
      warn "Dramatis::Actor included by #{cls}"
    end

    def self.extended cls
      warn "Dramatis::Actor included by #{cls}"
    end

    def self.inherited cls
      warn "Dramatis::Actor included by #{cls}"
    end
  end

end
