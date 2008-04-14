module Dramatis; end

class Dramatis::Error < StandardError; end
class Dramatis::Deadlock < Dramatis::Error; end
class Dramatis::BindError < Dramatis::Error; end
class Dramatis::Internal < Dramatis::Error; end

require 'dramatis/future/proxy'

module Dramatis

  def future *args, &block
    Dramatis::Future::Proxy.new( *args, &block )
  end

  module_function :future

  def name *args, &block
    Proxy.new( *args, &block )
  end

  module_function :name

  def cast name
    self.Name( name ).continue nil
  end

  module_function :name

  def self.included cls
    class << cls
      def new *args
        new_actor = Runtime::Actor.new
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

end
