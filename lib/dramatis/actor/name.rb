module Dramatis; end
module Dramatis::Actor; end
class Dramatis::Actor::Name; end

require 'dramatis/runtime/name_server'
require 'dramatis/actor'

class Dramatis::Actor::Name

  def to_s
    method_missing :to_s
  end

  def method_missing *args, &block
    block and @options[:block] = block
    @actor.object_send args, @options
  end

  private

  def initialize actor
    raise "hell" if !actor or !actor.kind_of? Dramatis::Runtime::Actor
    @actor = actor
    @options = { :continuation => :rpc }
    self
  end

end
