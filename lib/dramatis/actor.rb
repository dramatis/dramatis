module Dramatis; end
module Dramatis::Actor; end

require 'dramatis/runtime/name_server'
require 'dramatis/actor/name/proxy'
require 'dramatis/actor/name'

module Dramatis::Actor

  Runtime = Dramatis::Runtime
  NameServer = Runtime::NameServer
  Proxy = Dramatis::Actor::Name::Proxy

  def self.Name *args, &block
    Proxy.new( *args, &block )
  end

  def self.acts_as cls, opts = {}

    if opts[:new] != :object
      cls.class_eval do
        def self.new *args
          Dramatis::Actor.new super( *args )
        end
      end
    end

    cls.class_eval do
      def actor
        @__dramatis__ ||= Dramatis::Runtime::NameServer.the[self]
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
