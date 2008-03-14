module Dramatis; end
module Dramatis::Actor; end
class Dramatis::Actor::Name; end
class Dramatis::Actor::Name::Proxy; end

class Dramatis::Actor::Name::Proxyx

  def initialize *args
    @name = args[0]
    @opts = args[1]
  end

  def continue options, foo
    p "here"
    @name
  end

  def actor= object
    @name.instance_eval do
      @bind.call object
    end
    @name
  end

end

