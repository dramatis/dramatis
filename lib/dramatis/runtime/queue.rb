module Dramatis; end
module Dramatis::Runtime; end

class Dramatis::Runtime::Queue

  def initialize filter
    @queue = []
    @filter = filter
    @state = :blocked
  end

  def << task
    # FIX
    # I'm not sure about the locking here
    # as soon as the task is added to the queue and the queue
    # unlocked, the task could be run (we're not actor-protected here)
    # but for an RPC task, we may not have even gone into the wait yet
    lock {
      @queue << task
      if !runnable? and @filter.call( task )
        runnable! task
      end
    }
    task.queued
  end

  def runnable! task
    @state = :runnable
    @task = task
  end

  def runnable?
    @state == :runnable
  end

  def lock &block
    begin
      begin
        take_lock
      end
      block.call
      # finally
      release_lock
    end
  end

  def take_lock; end
  def release_lock; end

end
