module Dramatis; end
module Dramatis::Runtime; end

class Dramatis::Runtime::Queue

  def initialize filter
    @queue = []
    @filter = filter
    @state = :blocked
  end

  def << task
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
