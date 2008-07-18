module Dramatis; end
class Dramatis::Runtime; end

require 'dramatis/runtime/task'
require 'dramatis/runtime/gate'
require 'dramatis/runtime/timer'
require 'dramatis/actor/interface'
require 'dramatis/actor/name'
require 'thread'

begin require 'pp'; rescue Exception; end

class Dramatis::Runtime::Actor #:nodoc: all

  def name
    @name ||= Dramatis::Actor::Name.new self
  end

  attr_reader :object_interface
  attr_reader :object
  attr_reader :gate
  attr_reader :call_thread

  def initialize object = nil
    @call_threading = false
    @call_thread = nil
    @object = object
    @gate = Dramatis::Runtime::Gate.new
    @object_interface = Dramatis::Actor::Interface.new self
    if !object
      @gate.refuse :object
    else
      if Dramatis::Actor::Behavior === object
        if object.actor.name
          raise Dramatis::Error::Bind.new( "behavior already bound" )
        end
        eigenclass = ( class << object; self; end )
        eigenclass.send :undef_method, :actor
        oi = @object_interface
        eigenclass.send :define_method, :actor, ( lambda { oi } )
      end
    end
    @gate.always( ( [ :object, :dramatis_exception ] ), true )
    blocked!
    @queue = []
    @mutex = Mutex.new
    @continuations = {}
    Dramatis::Runtime::Scheduler.current << self
    if object.respond_to? :dramatis_bound
      object.send :dramatis_bound
    end
  end
  
  def become behavior
    return if behavior == @object
    if Dramatis::Actor::Behavior === behavior
      if behavior.actor.name
        raise Dramatis::Error::Bind.new( "cannot become bound behavior" )
      end
      eigenclass = ( class << behavior; self; end )
      eigenclass.send :undef_method, :actor
      oi = @object_interface
      eigenclass.send :define_method, :actor, ( lambda { oi } )
    end
    if Dramatis::Actor::Behavior === @object
      eigenclass = ( class << @object; self; end )
      eigenclass.send :undef_method, :actor
      eigenclass.send :define_method, :actor,
                ( lambda { Dramatis::Actor::Interface.new nil } )
    end
    @object = behavior
    if behavior.respond_to? :dramatis_bound
      behavior.send :dramatis_bound
    end
    # warn "x #{self}"
    schedule
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

  def yield; end

  def bind object
    raise Dramatis::Error::Bind if @object
    @object = object
    @gate.accept :object
    # warn "y #{self}"
    schedule
    name
  end

  def exception exception
    if @object.respond_to? :dramatis_exception
      @object.dramatis_exception exception
    else
      Dramatis::Runtime.current.exception exception
    end
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
    t = nil
    if opts[:continuation_send]
      t = :continuation 
      args.unshift opts[:continuation_send]
    else
      t = :object
    end
    common_send t, args, opts
  end

  def common_send dest, args, opts

    # warn "common send #{self} #{dest} #{args[0]}"
    # warn "common send #{self} #{dest} #{args.join(' ')} #{opts.to_a.join(' ' )}"

    task = Dramatis::Runtime::Task.new( self, dest, args, opts  )
    
    # warn "#{task.type} #{task.method}"
    # warn "#{self} #{Thread.current} common send r? #{runnable?} g? #{@gate.accepts?( *( [ task.type, task.method ] + task.arguments )  ) } q #{@queue.length}"
    @mutex.synchronize do
      # FIX arguments to gate
      if !runnable? and ( @gate.accepts?(  *( [ task.type, task.method ] + task.arguments ) ) or current_call_thread?( task.call_thread ) )
        runnable!
        # warn "a #{self} #{@state} #{task}"
        Dramatis::Runtime::Scheduler.current.schedule task
      else
        # warn "+>schd #{self} #{@queue.join(' ')}"
        @queue << task
        # warn "+<schd #{self} #{@queue.join(' ')}"
      end
    end

    task.queued

  end

  def deliver dest, args, continuation, call_thread
    # warn "b #{@state} #{dest} #{args} #{Thread.current} #{self}"
    if false and !runnable?
      warn caller.join("\n")
    end
    old_call_thread = @call_thread
    old_behavior_id = @object.object_id
    begin
      @call_thread = call_thread
      method = args.shift
      # warn "switch " + dest.to_s + " " + method.to_s
      result = 
        case dest
        when :actor
          # FIX: do name folding; needs test
          self.send method, *args
          # p "sent actor #{method}"
        when :object
          # warn "send object #{@object} #{method} #{args.length}"
          v = @object.send method, *args
          if v.object_id == @object.object_id
            v = name
          end
          # p "sent object #{method}"
          v
        when :continuation
          # FIX: name folding?
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

          if c.send(  method, *args )

            # the object no longer belongs to this thread; it belongs to the awoken thread
            # so don't try to reschedule it
            old_behavior_id = nil

          end

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
      if old_behavior_id == @object.object_id
        # warn "z #{@state} #{dest} #{args.join(' ')} #{Thread.current} #{self}"
        schedule
        # warn "za #{@state} #{dest} #{args} #{Thread.current} #{self}"
      end
    end
  end

  # note called from task.rb, too
  def schedule continuation = nil
    @mutex.synchronize do
      # warn ">schd #{self} #{@state} #{@queue.join(' ')}"
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
        Dramatis::Runtime::Scheduler.current.schedule task
      else
        blocked!
      end
      # warn "<schd #{self} #{task} #{Thread.current} #{@queue.join(' ')} #{@state} #{Thread.current}"
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

end

# Might be nice if this was broken out into another file ... YAGNI?
# This needs to be reworked. External threads and the main thread are
# related but different (there can only be one main thread). Mutliple
# at_exit calls are suspect. Of course, if the main actor tracks multilple
# runtimes, it might be okay.

class Dramatis::Runtime::Actor::Main < Dramatis::Runtime::Actor  #:nodoc: all

  class DefaultBehavior

    class Exception < ::Exception; end

    def method_missing *args
      raise Exception.new( "must use Actor#become to enable main actor" )
    end

    def dramatis_exception e
      if Dramatis::Runtime.current.warnings?
        warn "exception on main thread: #{e}"
        # pp caller
      end
      Dramatis::Runtime.current.exception e
    end

  end

  @@current = nil

  def self.current
    @@current ||= self.new
  end

  def self.reset
    @@current = nil
  end

  def quiesce
    schedule
  end

  def finalize
    if !@at_exit_run
      @at_exit_run = true
      # warn "zzz #{self}"
      schedule
      Dramatis::Runtime::Scheduler.current.main_at_exit
    end
  end

  def initialize
    super DefaultBehavior.new
    @at_exit_run = false
    at_exit { finalize }
    runnable!
  end

end
