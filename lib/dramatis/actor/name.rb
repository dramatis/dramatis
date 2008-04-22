module Dramatis; end
module Dramatis::Actor; end
class Dramatis::Actor::Name; end

require 'dramatis/actor'

# Dramatis::Actor::Names are proxy objects for actors. When a method
# is called on an actor name, the dramatis runtime creates and
# schedules an actor task to be run on the actors (virtual) thread.

# Dramatis::Actor::Name has no user-callable methods (except for the
# implicit method_missing). Other actor name operations are available
# through the Dramatis::Actor::Name::Interface object, accessible via
# Dramatis.interface.

class Dramatis::Actor::Name

  def to_s_off #:nodoc:
    method_missing :to_s
  end

  def dup #:nodoc:
    raise "hell again"
  end

  def method_missing *args, &block  #:nodoc:
    options = @options
    if block
      options = options.clone
      options[:block] = block
    end
    @actor.object_send args, options
  end

  private

  def initialize actor #:nodoc:
    raise "hell" if !actor or !actor.kind_of? Dramatis::Runtime::Actor
    @actor = actor
    @options = { :continuation => :rpc }
    self
  end

end
