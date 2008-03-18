module Dramatis; end
class Dramatis::Runtime; end

require 'dramatis/runtime/actor/main'
require 'dramatis/runtime'
require 'dramatis'
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
      # warn "sched #{@queue.length} #{@state}"
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
    # warn "maybe_deadlock #{Thread.current} #{Thread.main} threads #{@running_threads} queue #{@queue.length} #{Thread.list.join(" ")}"
    if @running_threads == 0 and @queue.length == 0 and @suspended_tasks.length > 0
      # deadlock
      begin
        raise Dramatis::Deadlock.new
      end
    end
  end

  def _deadlock
    false and @suspended_tasks.each_value do |task|
      task.exception Deadlock.new
    end
    @actors.each { |actor| actor.deadlock }
  end

  def checkio; false; end

  def suspend_notification task
    @mutex.synchronize do
      # deadlock if @state == :idle
      if @state == :idle
        @state = :running
        @running_threads = 1
        checkio and warn "#{Thread.current} checkout -1 #{Thread.main} #{@running_threads}"
        @thread = Thread.new { run }
      end
      checkio and warn "#{Thread.current} checkin 0 #{Thread.main} #{@running_threads}"
      @running_threads -= 1
      if @state == :waiting
        @wait.signal
      end
      @suspended_tasks[task.to_s] = task
    end
  end

  def wakeup_notification task
    @mutex.synchronize do
      @suspended_tasks.delete task.to_s
      @running_threads += 1
      checkio and warn "#{Thread.current} checkout #{@running_threads}"
    end
  end

  def quiesce
    main_at_exit
    @main_state = :running
  end

  def main_at_exit
    # warn "main has exited: waiting"
    @main_mutex.synchronize do
      checkio and warn "#{Thread.current} main checkin 1 #{@running_threads}"
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
          @main_join.join
          @main_join = nil
          raise "hell #{@main_state.to_s}" if @main_state != :may_finish
        else
          @main_state = :may_finish
        end
      end
    end
    raise "hell #{@main_state.to_s}" if @main_state != :may_finish
    # warn "?threads? #{Thread.list.join(' ')}"
    # warn "main has exited: done"
  end

  def << actor
    @actors << actor
  end

  private

  def initialize
    # Thread.abort_on_exception = true
    @mutex = Mutex.new
    @wait = ConditionVariable.new
    @running_threads = 0
    @suspended_tasks = {}
    @queue = []
    @state = :idle

    @main_mutex = Mutex.new
    @main_wait = ConditionVariable.new
    @main_state = :running

    @thread = nil

    @actors = []
  end

  class Done < ::Exception
  end

  def run
    
    checkio and warn "#{Thread.current} scheduler starting"

    begin
      while true
        task = nil
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
          actors.each { |actor| actor.deadlock deadlock }
        end
        @mutex.synchronize { maybe_deadlock }
    
        @mutex.synchronize do

          raise Done if @queue.empty? and @running_threads == 0

          if @queue.length > 0

            task = @queue.shift

            @running_threads += 1

            Thread.new do
              checkio and warn "#{Thread.current} spining up #{@running_threads}"
              begin
                deliver task
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
      warn "1 exception " + exception.to_s
      Dramatis::Runtime.the.exception exception
    end

    checkio and warn "scheduler giving up the ghost #{@queue.length} #{Thread.current}"

    begin
      @mutex.synchronize do
        maybe_deadlock
      end
    rescue Dramatis::Deadlock => deadlock
      actors = @mutex.synchronize { @actors.dup }
      actors.each { |actor| actor.deadlock }
    rescue Exception => exception
      warn "1 exception " + exception.to_s
      Dramatis::Runtime.the.exception exception
    end
    
    checkio and warn "scheduler giving up after deadlock #{@queue.length} #{Thread.current}"

    @main_mutex.synchronize do
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
      @mutex.synchronize do
        # warn "s #{Thread.current} m #{Thread.main} l #{Thread.list.join(' ')} "
        raise "hell" if @queue.length > 0
        @state = :idle
        @thread = nil
      end
    end

    checkio and warn "#{Thread.current} scheduler ending"

  end

  def deliver task
    thread = Thread.current
    thread[:dramatis_actor] = task.actor.name
    begin
      # p "deliver " + task.inspect
      task.deliver
      # p "delivered " + task.inspect
    rescue Exception => exception
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
