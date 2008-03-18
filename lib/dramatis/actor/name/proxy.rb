module Dramatis; end
module Dramatis::Actor; end
class Dramatis::Actor::Name; end

class Dramatis::Actor::Name::Proxy

  def initialize name
    raise "hell: " + name.inspect if !name or !name.kind_of? Dramatis::Actor::Name
    @name = name
  end

  # this stuff is either tricky or evil; i need to lookup
  # variable look ordering for instance_eval
  # i'm assuming lexical scope over object scope

  def continue options = {}, &continuation
    raise "hell" if ( options == nil and continuation ) or
                     ( options and !continuation )
    a, o = @name.instance_eval { [ @actor, @options ] }
    @name = Dramatis::Actor::Name.new a
    @name.instance_eval do
      @options = o.dup
      @options[:continuation] = options == nil ? :none : continuation
    end
    @name
  end

  def bind behavior
    actor_send :bind, behavior
  end

  def continuation c
    a, o = @name.instance_eval { [ @actor, @options ] }
    @name = Dramatis::Actor::Name.new a
    @name.instance_eval do
      @actor.register_continuation c
      @options = o.dup
      @options[:continuation_send] = c.to_s
      @options[:continuation] = :none
    end
    @name
  end

  private

  def actor_send *args, &block
    @name.instance_eval do
      options = @options
      if block
        options = options.dup
        options[:block] = block
      end
      @actor.actor_send args, options
    end
  end

end

