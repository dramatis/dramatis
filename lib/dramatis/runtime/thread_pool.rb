module Dramatis; end
class Dramatis::Runtime; end

class Dramatis::Runtime::ThreadPool; end
class Dramatis::Runtime::ThreadPool::PoolThread < Thread; end

class Dramatis::Runtime::ThreadPool #:nodoc: all

  def reset
    shutdown
    @state = :running
  end

  def initialize
    super
    @mutex = Mutex.new
    @threads = []
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

  private

  def shutdown
    @mutex.synchronize do
      @state = :exiting
      @threads.each do |thread|
        thread.send :shutdown
      end
      @threads.each do |thread|
        thread.join
      end
      @threads = []
    end
  end

  def checkin thread
    @mutex.synchronize do
      if @state != :exiting
        @threads << thread
      else
        warn("POST EXIT CHECKIN")
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
      end
      t = @threads.pop
    end
    t.send :awaken, &block
    t
  end
  
end

class Dramatis::Runtime::ThreadPool::PoolThread < Thread

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
          rescue Exception => e
            print_exc()
            raise
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
