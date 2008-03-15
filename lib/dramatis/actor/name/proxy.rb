module Dramatis; end
module Dramatis::Actor; end
class Dramatis::Actor::Name; end

class Dramatis::Actor::Name::Proxy

  def initialize name
    raise "hell" if !name or !name.kind_of? Dramatis::Actor::Name
    @name = name
  end

  def continue options = {}, &continuation
    raise "hell" if ( options == nil and continuation ) or
                     ( options and !continuation )
    @name.instance_eval do
      @options[:continuation] = options == nil ? :none : continuation
    end
    @name
  end

  def bind behavior
    actor_send :bind, behavior
    @name
  end

  private

  def actor_send *args, &block
    @name.instance_eval do
      block and @options[:block] = block
      @actor.actor_send args, @options
    end
  end

end

