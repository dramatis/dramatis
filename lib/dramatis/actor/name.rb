module Dramatis; end
module Dramatis::Actor; end
class Dramatis::Actor::Name; end

require 'dramatis/actor'

class Dramatis::Actor::Name

  def to_s_off
    method_missing :to_s
  end

  def dup
    raise "hell again"
  end

  def method_missing *args, &block
    options = @options
    if block
      options = options.clone
      options[:block] = block
    end
    @actor.object_send args, options
  end

  private

  def initialize actor
    raise "hell" if !actor or !actor.kind_of? Dramatis::Runtime::Actor
    @actor = actor
    @options = { :continuation => :rpc }
    self
  end

end
