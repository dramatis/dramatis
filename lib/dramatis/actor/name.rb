module Dramatis; end
module Dramatis::Actor; end
class Dramatis::Actor::Name; end

require 'dramatis/actor'
require 'dramatis/runtime/unbound'

Unbound = Dramatis::Runtime::Unbound

class Dramatis::Actor::Name

  def initialize object = nil
    @object = object || Unbound.new
  end

  def method_missing *args

    # debugging to make sure we don't send to the actor rather than the proxy
    if args[0].to_s =~ /^__.*__$/ 
      throw "a fit"
    end

    @object.send *args
  end

end
