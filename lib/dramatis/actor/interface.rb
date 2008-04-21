module Dramatis; end
module Dramatis::Actor; end

class Dramatis::Actor::Interface

  def refuse *args
    @actor.gate.refuse( :object, *args )
  end

  def accept *args
    @actor.gate.accept( :object, *args )
  end

  def default *args
    @actor.gate.default( [ :object ] + args )
  end

  def always args, value
    @actor.gate.always( ( [ :object ] + Array( args ) ), value )
  end

  def enable_call_threading
    @actor.enable_call_threading
  end

  def name
    @actor.name
  end

  def timeout value, *args
    @actor.timeout value, *args
  end

  private

  def gate
    @actor.gate
  end

  def initialize actor
    @actor = actor
  end

end
