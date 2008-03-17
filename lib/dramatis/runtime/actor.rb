module Dramatis; end
module Dramatis::Runtime; end

require 'dramatis/runtime/task'
require 'dramatis/runtime/gate'
require 'thread'

class Dramatis::Runtime::Actor

  @@anything = lambda { |*args| true; }
  @@nothing = lambda { |*args| false; }

  def initialize object = nil
    @object = object
    if object
      @gate = Dramatis::Runtime::Gate.new @@anything
    else
      @gate = Dramatis::Runtime::Gate.new @@nothing
    end
    @queue = []
    @mutex = Mutex.new
    @thread = nil
    @state = :blocked
  end
  
  def bind object
    raise RuntimeError.new( "a snit" ) if @object
    @object = object
    @gate.set @@anything
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

    task = Dramatis::Runtime::Task.new( self, dest, args, opts  )

    @mutex.synchronize do
      @queue << task

      if @state != :runnable and !@thread and @gate.accepts? task
        @state = :runnable
        Dramatis::Runtime::Scheduler.the.schedule task
      end
    end

    task.queued

  end

  def suspend continuation
    
  end

  def deliver dest, args, continuation
    @mutex.synchronize do
      @thread = Thread.current
    end
    begin
      method = args.shift
      result = 
        case dest
        when :actor
          self.send method, *args
        when :object
          @object.send method, *args
        else
          raise "hell: " + @dest.to_s
        end
      continuation.result result
    rescue => exception
      continuation.exception exception
    ensure
      @mutex.synchronize do
        @thread = nil
      end
    end
  end

  def accept *args, &block
    if args[0] == :nothing
      @filter = @@nothing
    end
  end

end
