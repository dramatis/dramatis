module Dramatis; end
module Dramatis::Actor; end
class Dramatis::Actor::Name; end
class Dramatis::Actor::Name::Proxy; end

class Dramatis::Actor::Name::Proxy

  def initialize *args
    @name = args[0]
    @opts = args[1]
  end

  def continue &block
    @name
  end

  def actor= object
    @name.instance_eval do
      @bind.call object
    end
    @name
  end

end

