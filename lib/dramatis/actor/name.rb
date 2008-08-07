require 'rubygems'
require 'builder/blankslate'

module Dramatis; end
module Dramatis::Actor; end

require 'dramatis/actor'

# Dramatis::Actor::Names are proxy objects for actors. When a method
# is called on an actor name, the dramatis runtime creates and
# schedules an actor task to be run on the actors (virtual) thread.

# Dramatis::Actor::Name has no user-callable methods (except for the
# implicit method_missing). Other actor name operations are available
# through the Dramatis::Actor::Name::Interface object, accessible via
# Dramatis.interface.

class Dramatis::Actor::Name < BlankSlate

  def _to_s #:nodoc:
    method_missing :to_s
  end

  def _respond_to? #:nodoc:
    method_missing :respond_to?
  end

  def _dup #:nodoc:
    raise "hell again"
  end

  def == other
    # p "== #{__id__} #{other.__id__}"
    if other.__id__ == nil.__id__
      return false
    end

    if other.__id__ == __id__
      return true
    end

    other_actor = other.instance_eval { @actor }

    return @actor == other_actor
  end

  def method_missing *args, &block  #:nodoc:
    # p "missing: #{args[0]}"
    # puts Kernel.caller.join("\n")
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
