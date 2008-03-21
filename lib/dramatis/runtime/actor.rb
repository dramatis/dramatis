module Dramatis; end
class Dramatis::Runtime; end

require 'dramatis/runtime/task'
require 'dramatis/runtime/gate'
require 'thread'
require 'pp' #FIX

class Dramatis::Runtime::Actor

  def name
    @name ||= Dramatis::Actor::Name.new self
  end

  attr_reader :object_interface
  attr_reader :gate

  def initialize object = nil
    @object = object
    if object
      @gate = Dramatis::Runtime::Gate.new true
    else
      @gate = Dramatis::Runtime::Gate.new false
    end
    blocked!
    @queue = []
    @mutex = Mutex.new
    @thread = nil
    @continuations = {}
    @object_interface = ObjectInterface.new self
    Dramatis::Runtime::Scheduler.the << self
  end
  
  def object_initialize *args
    @object.send :initialize, *args
  end

  def bind object
    # warn "bind #{object} #{@object} #{@gate}"
    raise Dramatis::BindError if @object
    @object = object
    # warn "okay?"
    # p @gate
    @gate.set_default true, :object
    # pp @gate
    self
  end

  def deadlock e
    @mutex.synchronize do
      @queue.each do |task|
        task.exception e
      end
      @queue.clear
    end
  end

  def register_continuation c
    # p "selfish", self, @continuations
    # pp "csr", c.to_s
    @continuations[c.to_s] = c
  end

  def actor_send args, opts
    common_send :actor, args, opts
  end

  def object_send args, opts
    if opts[:continuation_send]
      type = :continuation 
      begin
        raise "holly hell" if args[0] != :result and args[0] != :exception
      rescue Exception => exception
        pp exception.backtrace
      end
      args.unshift opts[:continuation_send]
    else
      type = :object
    end
    common_send type, args, opts
  end

  def common_send dest, args, opts

    warn "common send #{self} #{dest} #{args[0]}"
    # warn "common send #{self} #{dest} #{args.join(' ')} #{opts.to_a.join(' ' )}"

    task = Dramatis::Runtime::Task.new( self, dest, args, opts  )

    @mutex.synchronize do
      # warn "#{self} #{Thread.current} common send r? #{runnable?} g? #{@gate.accepts?( *( [ task.type, task.method ] + task.arguments )  ) } q #{@queue.length}"
      # FIX arguments to gate
      if !runnable? and @gate.accepts?(  *( [ task.type, task.method ] + task.arguments ) )
        runnable!
        Dramatis::Runtime::Scheduler.the.schedule task
      else
        # warn "+>schd #{self} #{@queue.join(' ')}"
        @queue << task
        # warn "+<schd #{self} #{@queue.join(' ')}"
      end
    end

    task.queued

  end

  def deliver dest, args, continuation
    @mutex.synchronize do
      @thread = Thread.current
    end
    begin
      method = args.shift
      # warn "switch " + dest.to_s + " " + method.to_s
      result = 
        case dest
        when :actor
          # p "send actor #{method}"
          self.send method, *args
          # p "sent actor #{method}"
        when :object
          # p "send object #{method}"
          @object.send method, *args
          # p "sent object #{method}"
        when :continuation
          # p "send continuation #{method}"
          continuation_name = method
          # warn "c is #{continuation_name}"
          c = @continuations[continuation_name]
          # pp "cs", @continuations.keys
          raise "hell 0 #{Thread.current}" if !c
          method = args.shift
          method = case method
                     when :result; :continuation_result
                     when :exception; :continuation_exception
                     else; raise "hell *"
                   end
          # pp c.to_s, "send", method, args
          c.send method, *args
          @continuations.delete continuation_name
          # pp "csd", continuation_name, @continuations.keys
        else
          raise "hell 1: " + @dest.to_s
        end
      # p "call c #{result}"
      # p continuation.to_s
      continuation.result result
      # p "called c #{result}"
    rescue Exception => exception
      pp "0 exception ", exception
      # pp exception.backtrace
      begin
        continuation.exception exception
      rescue Exception => e
        warn "double exception fault: #{e}"
        pp e
        raise e
      end
    ensure
      schedule
    end
  end

  # note called from task.rb, too
  def schedule
    @mutex.synchronize do
      # warn ">schd #{self} #{@queue.join(' ')}"
      @thread = nil
      task = nil
      index = 0
      while task == nil and index < @queue.length do
        candidate = @queue[index]
        # FIX arugments?
        if @gate.accepts?( *( [ candidate.type, candidate.method ] + candidate.arguments ) )
          task = candidate
          @queue[index,1] = []
        end
        index += 1
      end
      if task 
        Dramatis::Runtime::Scheduler.the.schedule task
      else
        blocked!
      end
      # warn "<schd #{self} #{@queue.join(' ')} #{@state} #{Thread.current}"
    end
  end

  def blocked!
    # warn "blocked! #{self} #{@state}"
    @state = :blocked
  end

  def runnable!
    # warn "runnable! #{self} #{@state}"
    @state = :runnable
  end

  def runnable?
    # warn "runnable? #{self} #{@state}"
    @state == :runnable
  end

  class ObjectInterface
    def gate
      @actor.gate
    end
    def refuse *args
      @actor.gate.refuse( :object, *args )
    end
    def accept *args
      @actor.gate.accept( :object, *args )
    end
    def name
      @actor.name
    end
    private
    def initialize actor
      @actor = actor
    end
  end

end

# Might be nice if this was broken out into another file ... YAGNI?

class Dramatis::Runtime::Actor::Main < Dramatis::Runtime::Actor

  class Object
    class Exception < ::Exception; end
    def method_missing
      raise Exception.new "must use Actor#become to enable main actor"
    end
  end

  def self.the
    @@the ||= self.new
  end

  def self.reset
    @@the = nil
  end

  def finalize
    schedule
    Dramatis::Runtime::Scheduler.the.main_at_exit
  end

  def initialize
    super Object.new
    at_exit { finalize }
  end

end
