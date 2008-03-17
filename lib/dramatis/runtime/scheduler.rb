module Dramatis; end
module Dramatis::Runtime; end

require 'thread'

class Dramatis::Runtime::Scheduler

  def self.the
    @@the ||= Dramatis::Runtime::Scheduler.new
  end

  def schedule task
    @mutex.synchronize do
      @queue << task
      if @queue.length == 1 && @state == :waiting
        @wait.signal
      end
    end
  end

  def suspend_notification task
  end

  private

  def initialize
    @mutex = Mutex.new
    @wait = ConditionVariable.new
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
            @wait.wait @mutex
            @state = :running
          end
          task = @queue.shift
        end
        Thread.new { deliver task }
      end
    rescue => error
      warn "scheduler exiting: " + error.inspect
    end

    @mutex.synchronize do
      @thread = nil
    end

  end

  def deliver task
    begin
      task.deliver
    rescue => error
      warn "attempt to deliver task failed: " + error.inspect
    end
  end


  @@the = self.new

end
