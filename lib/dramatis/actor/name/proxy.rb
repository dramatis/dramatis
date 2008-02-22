module Dramatis; end
module Dramatis::Actor; end
class Dramatis::Actor::Name; end
class Dramatis::Actor::Name::Proxy; end

class Dramatis::Actor::Name::Proxy

  def initialize *args, &block
    @name = args[0]
    @opts = args[1]
    @block = block
    self
  end

  def continue &block
    @name
  end

  def become object
    @name.__become__ object
  end

end
