module Dramatis; end

require 'thread'

# Dramatis::Runtime is the top level class managing the running of the
# various pieces of the dramatis runtime. Typically programs don't
# need to deal with the runtime directly, though some functions are
# useful, particularly for debugging and testing.

class Dramatis::Runtime

  # call-seq:
  # current -> current_runtime_object
  #
  # Returns a reference to the current Dramatis::Runtime object.

  def self.current
    @@current ||= self.new
  end

  # call-seq:
  # reset -> nil
  #
  # Resets the current runtime instance. Note that this method hard
  # resets counters and ignores exceptions which is generally a bad
  # idea. It is typical only used in unit test and spec "after"
  # methods to keep failing tests from cascading.

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

  # call-seq:
  # quiesce -> nil
  #
  # Causes the runtime to suspend the current thread until there are
  # no more tasks that can be executed. If no tasks remain, returns
  # normally. If tasks remain but are gated off,
  # Dramatis::Deadlock is raised.
  #
  # As a side effect, this method releases the current actor to
  # process messages but does not change the task gate.
  
  def quiesce
    Dramatis::Runtime::Scheduler.current.quiesce
    maybe_raise_exceptions true
  end

  def maybe_raise_exceptions quiescing #:nodoc:
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
        raise Dramatis::Error::Uncaught.new( @exceptions )
      end
      @exceptions.clear
    end
  end

  # call-seq:
  # exceptions -> array_of_exceptions
  #
  # Returns the list of exceptions that were not caught by an actor.

  def exceptions 
    result = nil
    @mutex.synchronize do
      result = @exceptions.dup
    end
    result
  end

  # call-seq:
  # clear_exceptions -> nil
  #
  # Clears the list of uncaught exceptions. Used in unit tests and specs to
  # clear expected exceptions.  If exceptions are raised and not
  # cleared, they will be raised at the end of the program via a
  # Dramatis::Error::Uncaught.
  
  def clear_exceptions
    @mutex.synchronize do
      # warn "runtime clearing exceptions"
      @exceptions.clear
    end
  end

  def exception exception #:nodoc:
    @mutex.synchronize do
      @exceptions << exception
      warn "runtime recording exception: #{exception} [#{@exceptions.length}]" if warnings?
      # backtrace
      # pp exception.backtrace
    end
  end

  def backtrace #:nodoc:
    begin
      raise "backtrace"
    rescue ::Exception => e
      pp e.backtrace
    end
  end

  # call-seq:
  # warnings = boolean -> boolean
  #
  # Enables or disables printing warnings, e.g., when uncaught
  # exceptions are detected. Returns the value passed.

  def warnings= value
    @warnings = value
  end

  # call-seq:
  # warnings? -> boolean
  #
  # Returns true if warnings are enabled.

  def warnings?
    @warnings
  end

  def at_exit #:nodoc:
    Dramatis::Runtime::Actor::Main.current.finalize
  end

  private

  def initialize #:nodoc:
    @warnings = true
    @mutex = Mutex.new
    @exceptions = []
  end

end
