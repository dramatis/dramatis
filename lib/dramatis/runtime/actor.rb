module Dramatis; end
module Dramatis::Runtime; end

require 'dramatis/runtime/queue'
require 'dramatis/runtime/task'

class Dramatis::Runtime::Actor

  @@anything = lambda { |*args| true; }
  @@nothing = lambda { |*args| false; }

  def initialize object = nil
    @object = object
    filter = nil
    if object
      filter = @@anything
    else
      filter = @@nothing
    end
    @queue = Dramatis::Runtime::Queue.new filter
  end
  
  def bind object
    raise RuntimeError.new( "a snit" ) if @object
    @object = object
    @queue.filter = @@anything
    self
  end

  def run_queue
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
  end

  def actor_send args, opts = {}
    common_send :actor, args, opts
  end

  def object_send args, opts = {}
    common_send :object, args, opts
  end

  def common_send dest, args, opts = {}
    @queue <<  Dramatis::Runtime::Task.new( self, dest, args, opts  )
  end

  def deliver dest, args, opts = {}
    p dest, args, opts
    raise "hell" if dest != :object and dest != :actor
    result = nil
    method = args.shift
    if dest == :object
      result = @object.send method, args
    else
      result = self.send method, args
    end
    return result
    p dest, args, opts
    raise "hell"
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
