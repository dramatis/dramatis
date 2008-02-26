module Dramatis; end
module Dramatis::Runtime; end

class Dramatis::Runtime::Actor

  @@anything = lambda { |*args| true; }
  @@nothing = lambda { |*args| false; }

  attr_reader :object

  def initialize object = nil
    @object = object
    @messages = []
    if object
      @filter = @@anything
    else
      @filter = @@nothing
    end
  end
  
  def bind object
    raise RuntimeError.new( "a snit" ) if @object
    @object = object
    @filter = @@anything
    run_queue
    self
  end

  def run_queue
    delivered = true
    while delivered do
      delivered = false
      @messages.each do |message|
        if @filter.call( *message )
          p "pop",  message
          @object.send *message
          @messages.shift
          delivered = true
          break
        end
      end
    end
  end

  def actor_send opts
    result = nil
    if @filter.call( *opts[:method_args] )
      result = @object.send *opts[:method_args]
      delivered = true
      while delivered do
        delivered = false
        @messages.each do |message|
          if @filter.call( *message )
            @object.send *message
            @messages.shift
            delivered = true
            break
          end
        end
      end
    else
      @messages.push opts[:method_args]
    end
    result
  end

  def accept *args, &block
    if args[0] == :nothing
      @filter = @@nothing
    end
  end

end
