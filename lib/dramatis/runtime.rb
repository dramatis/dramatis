module Dramatis; end

require 'thread'

class Dramatis::Runtime

  class Exception < ::Exception
    def initialize exceptions
      @exceptions = exceptions
    end
    def to_s
      super + ": " + @exceptions.join( " "  )
    end
  end

  def self.the
    @@the ||= self.new
  end

  def self.reset
    Dramatis::Runtime::Scheduler.reset    
    Dramatis::Runtime::Actor::Main.reset    
    @@the = nil
  end

  def quiesce
    Dramatis::Runtime::Scheduler.the.quiesce
    maybe_raise_exceptions
  end

  def maybe_raise_exceptions
    @mutex.synchronize do
      if !@exceptions.empty?
        warn "no maybe about it"
        raise Exception.new @exceptions
      end
      @exceptions.clear
    end
  end

  def exceptions
    result = nil
    @mutex.synchronize do
      result = @exceptions.dup
    end
    result
  end


  def clear_exceptions
    @mutex.synchronize do
      @exceptions.clear
    end
  end

  def exception exception
    warn "runtime recording exception: " + exception
    @mutex.synchronize do
      @exceptions << exception
    end
  end

  private

  def initialize
    @mutex = Mutex.new
    @exceptions = []
  end

end
