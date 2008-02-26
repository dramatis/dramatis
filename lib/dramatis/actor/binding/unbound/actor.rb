module Dramatis; end
module Dramatis::Actor; end
class Dramatis::Actor::Binding; end
module Dramatis::Actor::Binding::Unbound; end

class Dramatis::Actor::Binding::Unbound::Actor

  Dramatis::Actor::acts_as self

  def initialize binding
    @binding = binding
    binding.accept :nothing
  end

end
