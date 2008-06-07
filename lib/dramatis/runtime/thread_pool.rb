module Dramatis; end
class Dramatis::Runtime; end

class Dramatis::Runtime::ThreadPool; end
class Dramatis::Runtime::ThreadPool::PoolThread < Thread; end

class Dramatis::Runtime::ThreadPool #:nodoc: all

  def reset soft = false
    shutdown soft
    @state = :running
  end

  def initialize
    super
    @mutex = Mutex.new
    @wait = ConditionVariable.new
    @threads = []
    @size = 0
    @state = :running
  end

  def new &block
    checkout( &block )
  end

  def length
    @mutex.synchronize do
      @threads.length
    end
  end

  def size
    @mutex.synchronize do
      @size
    end
  end

  private

  def shutdown soft
    @mutex.synchronize do
      @state = :exiting
    end
    first = true
    @mutex.synchronize do
      while first or ( not soft and @size > 0 )
        first = false
        @threads.each do |thread|
          thread.send :shutdown
        end
        while thread = @threads.pop
          thread.true_join
          @size -= 1
        end
        if not soft and @size > 0
          @state = :exit_waiting
          @wait.wait @mutex
          @state = :exiting
        end
      end
    end
  end

  def checkin thread
    @mutex.synchronize do
      @threads << thread
      if @state == :exit_waiting
        @wait.signal
      end
    end
  end

  def checkout &block
    raise "hell" if @state == :exiting
    t = nil
    @mutex.synchronize do
      if @threads.length == 0
        pt = PoolThread.new self
        @threads << pt
        @size += 1
      end
      t = @threads.pop
    end
    t.send :awaken, &block
    t
  end
  
end

class Dramatis::Runtime::ThreadPool::PoolThread < Thread #:nodoc: all

  def initialize pool
    @pool = pool
    @mutex = Mutex.new
    @wait = ConditionVariable.new
    @state = :running
    super() do
      self.abort_on_exception = true
      target
    end
  end

  alias true_join join

  def join
    # I've thought about this. It would be cool to implement join. It's not
    # too hard to do it when threads aren't reused ... which is kinda dumb.
    # It's possible to do it when threads are reused, by coding an allocation
    # counter in the "thread" object (but not the native thread). It'd be
    # cool, but I don't need it, so, oh well.
    raise "not implemented"
  end

  private

  def shutdown
    @mutex.synchronize do
      old_state, @state = @state, :exiting
      # p "#{Thread.current} shutdown #{old_state} #{@state}"
      if old_state == :waiting
        @wait.signal
      end
    end
  end

  def awaken &block
    @mutex.synchronize do
      @block = block
      old_state, @state = @state, :called
      if old_state == :waiting
        # p "#{Thread.current} signalling"
        @wait.signal
      elsif old_state == :exiting
        warn("AWAKEN AFTER EXIT")
        raise "AWAKEN AFTER EXIT"
      elsif old_state != :running
        warn("AWAKEN BAD STATE " + old_state)
        raise "AWAKEN BAD STATE " + old_state
      end
    end
  end

  def target
    while true do
      @mutex.synchronize do
        if @state == :exiting
          return
        elsif @state == :running
          @state = :waiting
          @wait.wait @mutex
          raise "hell" if @state == :waiting
        elsif @state == :called
          begin
            @block.call
          ensure
            @state = :running
            @pool.send :checkin, self
          end
        else
          warn( "!!FAIL!! " + @state)
          raise "!!FAIL!! " + @state
        end
      end
    end
  end

end
