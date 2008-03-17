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
      @queue << task
      if @queue.length == 1 && @state == :waiting
        @wait.signal
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
      warn "suspend " + task.to_s + " before " + @running_threads.to_s + " queue " + @queue.length.to_s
      @running_threads -= 1
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
      warn "resume " + task.to_s + " after " + @running_threads.to_s  + " queue " + @queue.length.to_s
    end
  end

  private

  def initialize
    Thread.abort_on_exception = true
    @mutex = Mutex.new
    @wait = ConditionVariable.new
    @running_threads = 1
    @suspended_tasks = {}
    @queue = []
    @state = :running
    @thread = Thread.new { run }
  end

  def run

    begin
      loop do
        task = nil
        @mutex.synchronize do
          while @queue.empty?
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

          task = @queue.shift

          @running_threads += 1
          Thread.new do
            begin
              deliver task
            ensure
              @mutex.synchronize do
                @running_threads -= 1
              end
            end
          end

        end
      end
    rescue => exception
      warn "1 exception " + exception.to_s
      Dramatis::Runtime.the.exception exception
    end

    @mutex.synchronize do
      @thread = nil
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
