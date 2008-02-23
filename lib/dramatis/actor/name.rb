module Dramatis; end
module Dramatis::Actor; end
class Dramatis::Actor::Name; end

require 'dramatis/actor'
require 'dramatis/runtime/unbound'

class Dramatis::Actor::Name

  Runtime = Dramatis::Runtime
  Unbound = Runtime::Unbound

  def initialize object = nil
    if object
      @object = object
      @bind = lambda { throw "a fit" }
    else
      @object = Unbound.new
      @bind = lambda do |object|
        old = @object
        @object = object
        old.instance_eval {
          @bind.call( object )
        }
      end
    end
  end

  def method_missing *args

    # debugging to make sure we don't send to the actor rather than the proxy
    if args[0].to_s =~ /^__.*__$/ 
      throw "a fit"
    end

    Runtime::enqueue self, *args

  end

end
