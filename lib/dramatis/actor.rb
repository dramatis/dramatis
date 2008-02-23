module Dramatis; end
module Dramatis::Actor; end

require 'dramatis/actor/name/proxy/private'

module Dramatis::Actor

  Name = Dramatis::Actor::Name
  Proxy = Name::Proxy
  Private = Proxy::Private

  def self.Name *args, &block
    Proxy.new *args, &block
  end

  def self.acts_as klass

    klass.class_eval do
      def actor
        @name ||= Private.new( Name.new( self ) )
      end
    end

  end

end
