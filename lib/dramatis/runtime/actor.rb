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
  attr_reader :call_thread

  def initialize object = nil
    @call_threading = false
    @call_thread = nil
    @object = object
    @gate = Dramatis::Runtime::Gate.new
    if !object
      @gate.refuse :object
    end
    blocked!
    @queue = []
    @mutex = Mutex.new
    @continuations = {}
    @object_interface = ObjectInterface.new self
    Dramatis::Runtime::Scheduler.the << self
  end
  
  def call_threading?
    @call_threading
  end

  def enable_call_threading
    @call_threading = true
  end

  def current_call_thread? that
    # warn "current_call_thread? #{@call_thread} #{(@call_thread and (@call_thread == that)).inspect} #{that}"
    @call_thread and @call_thread == that
  end

  def object_initialize *args
    @gate.accept :object
    @object.send :initialize, *args
  end

  def bind object
    # warn "bind #{object} #{@object} #{@gate}"
    raise Dramatis::BindError if @object
    @object = object
    # warn "okay?"
    # p @gate
    @gate.accept :object
    # pp @gate
    self
  end

  def deadlock e
    tasks = nil
    @mutex.synchronize do
      tasks = @queue.dup
      @queue.clear
    end
    tasks.each { |task| task.exception e }
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

    # warn "common send #{self} #{dest} #{args[0]}"
    # warn "common send #{self} #{dest} #{args.join(' ')} #{opts.to_a.join(' ' )}"

    task = Dramatis::Runtime::Task.new( self, dest, args, opts  )

    # warn "#{self} #{Thread.current} common send r? #{runnable?} g? #{@gate.accepts?( *( [ task.type, task.method ] + task.arguments )  ) } q #{@queue.length}"
    begin
      # raise "helly"
    rescue Exception => e
      pp e.backtrace
    end
    @mutex.synchronize do
      # FIX arguments to gate
      if !runnable? and ( @gate.accepts?(  *( [ task.type, task.method ] + task.arguments ) ) or current_call_thread?( task.call_thread ) )
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

  def deliver dest, args, continuation, call_thread
    old_call_thread = @call_thread
    begin
      @call_thread = call_thread
      method = args.shift
      # warn "switch " + dest.to_s + " " + method.to_s
      result = 
        case dest
        when :actor
          # p "send actor #{method}"
          self.send method, *args
          # p "sent actor #{method}"
        when :object
          # p "send object #{@object} #{method}"
          v = @object.send method, *args
          # p "sent object #{method}"
          v
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
      # p "call c '#{result}'"
      # p continuation.to_s
      continuation.result result
      # p "called c #{result}"
    rescue Exception => exception
      # pp "0 exception ", exception
      # pp exception.backtrace
      begin
        continuation.exception exception
      rescue Exception => e
        warn "double exception fault: #{e}"
        pp e
        raise e
      end
    ensure
      @call_thread = old_call_thread
      schedule
    end
  end

  # note called from task.rb, too
  def schedule continuation = nil
    @mutex.synchronize do
      # warn ">schd #{self} #{@queue.join(' ')}"
      task = nil
      index = 0
      while task == nil and index < @queue.length do
        candidate = @queue[index]
        # FIX arugments?
        if @gate.accepts?( *( [ candidate.type, candidate.method ] + candidate.arguments ) ) or
           current_call_thread? candidate.call_thread
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
    def default *args
      @actor.gate.default( [ :object ] + args )
    end
    def always args, value
      @actor.gate.always( ( [ :object ] + Array( args ) ), value )
    end
    def enable_call_threading
      @actor.enable_call_threading
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
# This needs to be reworked. External threads and the main thread are
# related but different (there can only be one main thread). Mutliple
# at_exit calls are suspect. Of course, if the main actor tracks multilple
# runtimes, it might be okay.

class Dramatis::Runtime::Actor::Main < Dramatis::Runtime::Actor

  class Object
    class Exception < ::Exception; end
    def method_missing
      raise Exception.new( "must use Actor#become to enable main actor" )
    end
  end

  def self.the
    @@the ||= self.new
  end

  def self.reset
    @@the = nil
  end

  def finalize
    if !@at_exit_run
      @at_exit_run = true
      schedule
      Dramatis::Runtime::Scheduler.the.main_at_exit
    end
  end

  def initialize
    super Object.new
    @at_exit_run = false
    at_exit { finalize }
  end

end
