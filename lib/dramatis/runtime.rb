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

  @@current = nil

  def self.current
    @@current ||= self.new
  end

  def self.reset
    # this swallows exceptions: it's assumed to be used to clean up
    # a failed test so there's no connection between tests
    begin
      Dramatis::Runtime.current.quiesce
    rescue Exception => e
    end
    Dramatis::Runtime::Scheduler.reset    
    Dramatis::Runtime::Actor::Main.reset    
    @@current = nil
  end

  def quiesce
    Dramatis::Runtime::Scheduler.current.quiesce
    maybe_raise_exceptions true
  end

  def maybe_raise_exceptions quiescing
    @mutex.synchronize do
      if !@exceptions.empty?
        # warn "no maybe about it"
        if !quiescing and warnings?
          warn "the following #{@exceptions.length} exception(s) were raised and not caught"
          @exceptions.each do |exception|
            # warn "#{exception}"
            # pp exception.backtrace
          end
        end
        raise Exception.new( @exceptions )
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
      # warn "runtime clearing exceptions"
      @exceptions.clear
    end
  end

  def exception exception
    if false
      begin
        raise "hell"
      rescue ::Exception => e
        pp e.backtrace
      end
    end
    @mutex.synchronize do
      @exceptions << exception
      warn "runtime recording exception: #{exception} [#{@exceptions.length}]" if warnings?
      # backtrace
      # pp exception.backtrace
    end
  end

  def backtrace
    begin
      raise "backtrace"
    rescue ::Exception => e
      pp e.backtrace
    end
  end

  def warnings= value
    @warnings = value
  end

  def warnings?
    @warnings
  end

  def at_exit
    Dramatis::Runtime::Actor::Main.current.finalize
  end

  private

  def initialize
    @warnings = true
    @mutex = Mutex.new
    @exceptions = []
  end

end
