module Dramatis; end
module Dramatis::Actor; end

require 'dramatis/actor/name/proxy'

Proxy = Dramatis::Actor::Name::Proxy

module Dramatis::Actor

  def self.Name *args, &block
    Proxy.new *args, &block
  end

end
