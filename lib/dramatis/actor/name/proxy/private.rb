module Dramatis; end
module Dramatis::Actor; end
class Dramatis::Actor::Name; end
class Dramatis::Actor::Name::Proxy; end

require 'dramatis/actor/name/proxy'

class Dramatis::Actor::Name::Proxy::Private < Dramatis::Actor::Name::Proxy

  def initialize *args
    super( *args )
  end

  def accept *args
  end

end

