module Dramatis; end
class Dramatis::Runtime; end

require 'dramatis/runtime/actor/main'
require 'dramatis/runtime'
require 'thread'
require 'pp'

class Dramatis::Runtime::Scheduler

  def self.reset
    @@the = nil
  end

  def self.the
    @@the ||= self.new
  end

  def schedule task
    @mutex.synchronize do
      warn "sched #{@queue.length} #{@state}"
      @queue << task
      if @queue.length == 1
        if @state == :waiting
          @wait.signal
        elsif @state == :idle
          @state = :running
          @thread = Thread.new { run }
        end
      end
    end
  end

  class Deadlock < ::Exception
  end

  def deadlock
    @suspended_tasks.each_value do |task|
      task.exception Deadlock.new
    end
    raise Deadlock.new
  end

  def suspend_notification task
    @mutex.synchronize do
      deadlock if @state == :idle
      warn "#{Thread.current} checkin #{@running_threads}"
      @running_threads -= 1
      raise "hell #{@state}" if @state == :idle
      if @state == :waiting
        @wait.signal
      elsif @state == :idle
        @state = :running
        @thread = Thread.new { run }
      end
      # FIX empty queues
      if @running_threads == 0
        deadlock
      end
      @suspended_tasks[task.to_s] = task
    end
  end

  def wakeup_notification task
    @mutex.synchronize do
      @suspended_tasks.delete task.to_s
      @running_threads += 1
      warn "#{Thread.current} checkout #{@running_threads}"
    end
  end

  def quiesce
    main_at_exit
    @main_state = :running
  end

  def main_at_exit
    warn "main has exited: waiting"
    @main_mutex.synchronize do
      warn "#{Thread.current} main checkin #{@running_threads}"
      @running_threads -= 1
      if @state == :waiting
        @wait.signal
      end
    end
    # sleep 15
    @main_mutex.synchronize do
      raise "hell #{@main_state.to_s}" if @main_state != :running and @main_state != :may_finish
      if @main_state == :running
        if @state != :idle
          @main_state = :waiting
          @main_wait.wait @main_mutex
          raise "hell #{@main_state.to_s}" if @main_state != :may_finish
        else
          @main_state = :may_finish
        end
      end
    end
    raise "hell #{@main_state.to_s}" if @main_state != :may_finish
    warn "main has exited: done"
  end

  private

  def initialize
    Thread.abort_on_exception = true
    @mutex = Mutex.new
    @wait = ConditionVariable.new
    warn "#{Thread.current} checkout main #{Thread.main}"
    @running_threads = 1
    @suspended_tasks = {}
    @queue = []
    @state = :idle

    @main_mutex = Mutex.new
    @main_wait = ConditionVariable.new
    @main_state = :running

    @thread = nil
  end

  class Done < ::Exception
  end

  def run

    begin
      while true
        task = nil
        @mutex.synchronize do
          warn "qe #{@queue.empty?} tr '#{@running_threads}'"
          while @queue.empty? and @running_threads != 0
            @state = :waiting
            begin
              if @running_threads == 0
                deadlock
              end
              @wait.wait @mutex
            rescue => exception
              warn "wait exception: #{exception}"
            end
            @state = :running
          end
          
          raise Done if @queue.empty? and @running_threads == 0

          task = @queue.shift

          @running_threads += 1
          Thread.new do
            warn "#{Thread.current} checkout #{@running_threads}"
            begin
              deliver task
            ensure
              @mutex.synchronize do
                warn "#{Thread.current} checkin #{@running_threads}"
                @running_threads -= 1
                if @state == :waiting
                  @wait.signal
                end
              end
            end
          end

        end
      end
      warn "after loop"
    rescue Done
    rescue => exception
      warn "1 exception " + exception.to_s
      Dramatis::Runtime.the.exception exception
    end

    warn "scheduler giving up the ghost"
    
    @main_mutex.synchronize do
      raise "hell #{@main_state.to_s}" if @main_state != :running and @main_state != :waiting
      state = @main_state
      @main_state = :may_finish
      if state == :waiting
        @main_wait.signal
      end
      # should this be synchronized?
      # if there is more than one main (non-dramatis) thread ... well, I'm not sure
      # how protected we are for that around shutdown/startup
      # I guess it won't hurt, but I'm not sure it helps
      @mutex.synchronize do
        @state = :idle
        @thread = nil
      end
    end

  end

  def deliver task
    thread = Thread.current
    thread[:dramatis_actor] = task.actor.name
    begin
      # p "deliver " + task.inspect
      task.deliver
      # p "delivered " + task.inspect
    rescue => exception
      warn "2 exception " + exception.to_s
      pp exception.backtrace
      Dramatis::Runtime.the.exception exception
    ensure
      thread[:dramatis_actor] = nil
    end
  end

  def self.current
    thread = Thread.current
    actor = thread[:dramatis_actor]
    if !actor
      if thread == Thread.main
        actor = Dramatis::Runtime::Actor::Main.the.name
      end
    else
      # this is a debugging path; can go away
      raise "hell" if thread == Thread.main and
                       actor != Dramatis::Runtime::Actor::Main.the
      raise "hell" if thread != Thread.main and
                       actor == Dramatis::Runtime::Actor::Main.the
    end
    actor
  end

end