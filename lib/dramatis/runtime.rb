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
    maybe_raise_exceptions true
  end

  def maybe_raise_exceptions quiescing
    @mutex.synchronize do
      if !@exceptions.empty?
        # warn "no maybe about it"
        if !quiescing and warnings?
          warn "the following #{@exceptions.length} exception(s) were raised and not caught"
          @exceptions.each do |exception|
            warn "#{exception}"
            pp exception.backtrace
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
      warn "runtime recording exception: #{exception} #{@exceptions.length}" if warnings?
    end
  end

  def warnings= value
    @warnings = value
  end

  def warnings?
    @warnings
  end

  def at_exit
    Dramatis::Runtime::Actor::Main.the.finalize
  end

  private

  def initialize
    @warnings = true
    @mutex = Mutex.new
    @exceptions = []
  end

end
