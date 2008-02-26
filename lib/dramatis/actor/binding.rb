module Dramatis; end
module Dramatis::Actor; end
class Dramatis::Actor::Binding; end

require 'dramatis/actor'
require 'dramatis/actor/binding/unbound/actor'

class Dramatis::Actor::Binding

  @@anything = lambda { |*args| true; }
  @@nothing = lambda { |*args| false; }

  attr_reader :object

  def initialize object = nil
    @filter = @@anything
    @object = object || Unbound::Actor.new( self )
  end
  
  def bind object
    raise RuntimeError.new "a snit"
  end

  def eval *args
    if @filter.call( *args )
      @object.send *args
    end
  end

  def accept *args, &block
    if args[0] == :nothing
      @filter = @@nothing
    end
  end

end
