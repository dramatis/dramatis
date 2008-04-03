module Dramatis; end
module Dramatis::Actor; end
class Dramatis::Actor::Name; end

class Dramatis::Actor::Name::Proxy

  def initialize name
    raise "hell: " + name.inspect \
      if !name or !name.kind_of? Dramatis::Actor::Name
    @name = name
  end

  # this stuff is either tricky or evil; i need to lookup
  # variable look ordering for instance_eval
  # i'm assuming lexical scope over object scope

  def continue options = {}, &continuation
    raise "contradictory options passed to continue" \
        if ( options == nil and continuation ) or
           ( options and !continuation )
    a, o = @name.instance_eval { [ @actor, @options ] }
    @name = Dramatis::Actor::Name.new a
    @name.instance_eval do
      @options = o.dup
      @options[:continuation] = options == nil ? :none : continuation
      options and @options[:exception] = options[:exception]
    end
    @name
  end

  def future
    a, o = @name.instance_eval { [ @actor, @options ] }
    @name = Dramatis::Actor::Name.new a
    @name.instance_eval do
      @options = o.dup
      @options[:continuation] = :future
    end
    @name
  end

  def bind behavior
    actor_send :bind, behavior
  end

  def exception exception
    actor_send :exception, exception
  end

  def url
    "http://something"
  end

  private

  def continuation c, options
    a, o = @name.instance_eval { [ @actor, @options ] }
    @name = Dramatis::Actor::Name.new a
    @name.instance_eval do
      @actor.register_continuation c
      @options = o.dup
      @options[:continuation_send] = c.to_s
      @options[:continuation] = :none
      # FIX merge options, rather than cherry-pck
      options[:call_thread] and @options[:call_thread] = options[:call_thread]
    end
    @name
  end

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

