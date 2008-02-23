module Dramatis; end
module Dramatis::Runtime; end

class Dramatis::Runtime::Unbound

  Name = Dramatis::Actor::Name
  Private = Name::Proxy::Private

  Dramatis::Actor.acts_as self

  def initialize

    actor.accept :nothing

    @bind = lambda do |object|
      @name = Private.new( Name.new( object ) )
      actor.accept :anything
    end

  end

end
