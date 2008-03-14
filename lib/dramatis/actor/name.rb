module Dramatis; end
module Dramatis::Actor; end
class Dramatis::Actor::Name; end

require 'dramatis/runtime/name_server'
require 'dramatis/actor'

class Dramatis::Actor::Name

  def initialize object = nil
    @binding = Dramatis::Runtime::NameServer.the.new object
    @options = nil
    return self
  end

  def to_s
    method_missing :to_s
  end

  def method_missing *args
    return @binding.actor_send( :method_args => args,
                                :name_args => @options )
  end

end
