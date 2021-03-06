module Darmatis; end
class Dramatis::Runtime; end

require 'thread'

require 'dramatis/runtime/thread_pool'
require 'dramatis/runtime/actor/main'
require 'dramatis/runtime'
require 'dramatis'

begin require 'pp'; rescue Exception; end

class Dramatis::Runtime::Scheduler #:nodoc: all

  def checkio; false; end

  def self.reset
    @@current.reset
    @@current = nil
  end

  @@current = nil

  def self.current
    @@current ||= self.new
  end

  def reset
    @thread_pools.each do |thread_pool|
      thread_pool.reset
    end
  end

  def schedule task
    @mutex.synchronize do
      # warn "sched #{@queue.length} #{@state} #{task}"
      begin
        raise "bad bad bad" if task == nil
      rescue Exception => e
        p "very bad very #{e}"
        pp e.backtrace
        raise e
      end
      @queue << task
      if @queue.length == 1
        if @state == :waiting
          @wait.signal
        elsif @state == :idle
          @state = :running
          @running_threads = 1
          checkio and warn "#{Thread.current} checkout main #{Thread.main} #{@running_threads}"
          @thread = Thread.new { run }
        end
      end
    end
  end

  # must be called with @mutex locked
  # must be called after @running_threads decremented
  def maybe_deadlock
    # warn "maybe_deadlock #{Thread.current} #{Thread.main} threads #{@running_threads} queue #{@queue.length} #{Thread.list.join(" ")} qg #{@quiescing} scl #{@suspended_continuations.length}"
    if @running_threads == 0 and @queue.length == 0 and @suspended_continuations.length > 0 and !@quiescing
      # p "DEADLOCK!"
      raise Dramatis::Deadlock.new
    end
  end

  def suspend_notification continuation
    @mutex.synchronize do
      if @state == :idle
        @state = :running
        @running_threads = 1
        checkio and warn "#{Thread.current} checkout -1 #{Thread.main} #{@running_threads}"
        @thread = Thread.new { run }
      end
      checkio and warn "#{Thread.current} checkin 0 #{Thread.main} #{@running_threads}"
      @running_threads -= 1
      begin
        raise "cane" if @running_threads < 0
      rescue ::Exception => e
        pp e.backtrace
      end
      if @state == :waiting
        @wait.signal
      end
      @suspended_continuations[continuation.to_s] = continuation
    end
  end

  def wakeup_notification continuation
    @mutex.synchronize do
      @suspended_continuations.delete continuation.to_s
      @running_threads += 1
      checkio and warn "#{Thread.current} checkout #{@running_threads}"
    end
  end

  def quiesce
    Dramatis::Runtime::Actor::Main.current.quiesce
    main_at_exit true
  end

  def main_at_exit quiescing = false
    # warn "quiescing" if quiescing
    # warn "main has exited: waiting" if !quiescing
    @mutex.synchronize do
      @quiescing = quiescing
      checkio and warn "#{Thread.current} main maybe checkin 1 #{@running_threads} #{@state} #{@main_state} #{quiescing}"
      if @state != :idle
        @running_threads -= 1
        if @state == :waiting
          @wait.signal
        end
      end

      raise "hell #{@main_state.to_s}" if @main_state != :running and @main_state != :may_finish
      checkio and warn "#{Thread.current} main signaled #{@running_threads} #{@state} #{@main_state} #{quiescing}"

      @main_mutex.synchronize do
      if @main_state == :running
        if @state != :idle
          @main_state = :waiting
          begin
            @main_wait.wait @mutex
          rescue Exception => e
            # pp "wait said #{e}", e.backtrace
            raise e
          end
          @main_join.join
          @main_join = nil
          raise "hell #{@main_state.to_s}" if @main_state != :may_finish
        else
          begin
            maybe_deadlock
          rescue Dramatis::Deadlock => deadlock
            warn "Deadlock at exit: uncompleted tasks exist"
            raise deadlock
          end
          @main_state = :may_finish
        end
      end
      @main_state = :running if @quiescing
      end
      @quiescing = false
    end
    raise "hell #{@main_state.to_s}" if @main_state != :may_finish and @main_state != :running
    # warn "?threads? #{Thread.list.join(' ')}"
    # warn "main has exited: done"
    @thread_pool.reset quiescing
    Dramatis::Runtime.current.maybe_raise_exceptions quiescing
  end

  def << actor
    @actors << actor
  end

  def thread_count
    @mutex.synchronize do
      @thread_pools.inject(0) { |a,b| a+b.size }
    end
  end

  private

  def initialize
    @thread_pool = Dramatis::Runtime::ThreadPool.new
    @thread_pools = [ @thread_pool ]
    # Thread.abort_on_exception = true
    @mutex = Mutex.new
    @wait = ConditionVariable.new
    @running_threads = 0
    @suspended_continuations = {}
    @queue = []
    @state = :idle

    @main_mutex = Mutex.new
    @main_wait = ConditionVariable.new
    @main_state = :running
    @quiescing = false

    @thread = nil

    @actors = []
  end

  class Done < ::Exception
  end

  def run
    
    checkio and warn "#{Thread.current} scheduler starting #{@state}"

    begin
      while true
        @mutex.synchronize do
          # warn "qe #{@queue.empty?} tr '#{@running_threads}'"
          while @queue.empty? and @running_threads != 0
            @state = :waiting
            begin
              raise "hell" if @running_threads == 0
              @wait.wait @mutex
            rescue Exception => exception
              warn "wait exception: #{exception}"
            ensure
              @state = :running
            end
          end
        end
          
        begin
          @mutex.synchronize { maybe_deadlock }
        rescue Dramatis::Deadlock => deadlock
          actors = @mutex.synchronize { @actors.dup }
          thread = Thread.current
          actors.each do |actor|
            thread[:dramatis_actor] = actor.name
            actor.deadlock deadlock
          end
          thread[:dramatis_actor] = nil
        end

        @mutex.synchronize { maybe_deadlock }
    
        @mutex.synchronize do

          raise Done if @queue.empty? and @running_threads == 0

          if @queue.length > 0

            task = @queue.shift
            
            # p "task #{task}"

            begin
              raise "hell!!!!" if task == nil
            rescue Exception => e
              p "bad bad #{e}"
              pp e.backtrace
              raise e
            end

            @running_threads += 1

            # p "tasket #{task}"
            
            @thread_pool.new do

              # "tasky", task

              # "tasketx #{task}"

              checkio and warn "#{Thread.current} spining up #{@running_threads}"
              begin
                deliver task
              rescue Exception => e
                warn "unexptected deliver error #{e}"
                raise e
              ensure
                @mutex.synchronize do
                  checkio and warn "#{Thread.current} checkin 2 #{@running_threads} #{@state}"
                  @running_threads -= 1
                  if @state == :waiting
                    @wait.signal
                  else
                    # maybe_deadlock
                  end
                end
              end
              checkio and warn "#{Thread.current} retiring #{@running_threads}"
            end
          end
        end
      end
      # warn "after loop"
    rescue Done
    rescue Exception => exception
      warn "1 *? exception " + exception.to_s
      warn "smp!!! " + exception.backtrace.join("\n")
      # pp exception.backtrace
      Dramatis::Runtime.current.exception exception
    end

    checkio and warn "scheduler giving up the ghost #{@queue.length} #{Thread.current}"

    begin
      @mutex.synchronize do
        maybe_deadlock
      end
    rescue Dramatis::Deadlock => deadlock
      actors = @mutex.synchronize { @actors.dup }
      actors.each { |actor| actor.deadlock deadlock }
    rescue Exception => exception
      warn "2 exception " + exception.to_s
      Dramatis::Runtime.current.exception exception
    end
    
    checkio and warn "scheduler giving up after final deadlock check #{@queue.length} #{Thread.current}"

    # FIX need to check all the mutex nesting between main_mutex and mutex
    # I think this should be the main mutex ...
    @mutex.synchronize do
      raise "hell #{@main_state.to_s}" if @main_state != :running and @main_state != :waiting
      state = @main_state
      @main_state = :may_finish
      # warn "main is #{state}"
      if state == :waiting
        @main_join = Thread.current
        @main_wait.signal
      end
      # should this be synchronized?
      # if there is more than one main (non-dramatis) thread ... well, I'm not sure
      # how protected we are for that around shutdown/startup
      # I guess it won't hurt, but I'm not sure it helps
      # @mutex.synchronize do
        # warn "s #{Thread.current} m #{Thread.main} l #{Thread.list.join(' ')} "
        raise "hell" if @queue.length > 0
        @state = :idle
        @thread = nil
    # end
    end

    checkio and warn "#{Thread.current} scheduler ending"

  end

  def deliver task
    thread = Thread.current
    begin
      raise "hel!!!" if !task
    rescue Exception => e
      p "very bad!! #{e}"
      pp e.backtrace
      raise e
    end
    thread[:dramatis_actor] = task.actor.name
    xx = task.actor.name
    xx = xx.instance_eval { @actor }
    # warn "assign #{xx} to #{thread}"
    begin
      # warn "deliver " + task.inspect
      task.deliver
      # warn "delivered " + task.inspect
    rescue Exception => exception
      warn "2 exception " + exception.to_s
      # pp exception.backtrace
      Dramatis::Runtime.current.exception exception
    ensure
      thread[:dramatis_actor] = nil
    end
  end

  def self.actor
    thread = Thread.current
    actor = thread[:dramatis_actor]
    if !actor
      if thread == Thread.main
        # p "here", actor
        actor = Dramatis::Runtime::Actor::Main.current.name
        # p "there", actor
      else
        if !actor = thread[:dramatis_exogneous_actor]
          actor = thread[:dramatis_exogneous_actor] =
            Dramatis::Runtime::Actor::Exogneous.new.name
        end
      end
    else
      # this is a debugging path; can go away
      raise "hell" if thread == Thread.main and
                       actor != Dramatis::Runtime::Actor::Main.current
      raise "hell" if thread != Thread.main and
                       actor == Dramatis::Runtime::Actor::Main.current
    end
    actor
  end

end
