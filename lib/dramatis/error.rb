module Dramatis; end

# The base class of all non-internal dramatis exceptions.

class Dramatis::Error < StandardError; end

# Raised when an attempt is made to create an interface object on an
# object that does not define an interface class.

class Dramatis::Error::Interface < StandardError; end

# Raised when an attempt is made to bind an already-bound actor.

class Dramatis::Error::Bind < Dramatis::Error; end

require 'dramatis/error/uncaught'

class Dramatis::Error < StandardError; end

# Dramatis modifies exceptions thrown within the context of an actor rpc call,
# combinding the the backtraces generated by native language exceptions in
# order to put them in a more useful context:
# 1. Exceptions are chained across threads using continuation information
# 1. Dramatis runtime internal call frames are removed

class Exception

  # :stopdoc:
  alias _dramatis_backtrace backtrace
  # :startdoc:

  # call-seq:
  # backtrace -> array of strings
  #
  # dramatis wraps and filters the native exception backtrace method in order to
  # augment the backtrace to follow the backtrace through actor rpc calls.
  #

  def backtrace
    # p "back #{self}"
    if instance_variable_defined? :@_dramatis_raw_backtrace
      filter @_dramatis_raw_backtrace
    else
      _dramatis_backtrace
    end
  end

  def _dramatis_reraise  #:nodoc:
    # return
    # p "reraise"
    if @_dramatis_raw_backtrace
      @_dramatis_raw_backtrace = backtrace + filter( caller )
    else
      @_dramatis_raw_backtrace = filter( backtrace ) + filter( caller )
    end
  end

  def filter array  #:nodoc:

    # pp "_", array

    filtered = []
    array.each do |v|
      file, line, func = v.split ':'
      file =~ %r{/lib/dramatis/} or ( filtered <<  v and next )
      func =~ %r{\Wmaybe_deadlock\W} and next
      file =~ %r{/runtime/scheduler} and func =~ %r{\Wrun\W} and break
      filtered <<  v
    end
    
    # pp "0", filtered
    
    # remove queueing delivery

    array = filtered
    filtered = []
    skipping = false
    array.each do |v|

      # p v

      file, line, func = v.split ':'

      if file !~ %r{/lib/dramatis/}
        # p "not skipping x"
        skipping = false
        filtered <<  v
        next
      end

      if !skipping and 
         ( ( file =~ %r{/runtime/task} and func =~ %r{\Wqueued\W} ) or
           ( file =~ %r{/runtime/actor} and func =~ %r{\Wsend\W} ) or # r18, r19
           ( file =~ %r{/runtime/actor} and func =~ %r{\Wdeliver\W} )  ) # jr
        # p "skipping"
        skipping = true
        next
      end

      if file =~ %r{/dramatis/actor/name} and func =~ %r{\Wmethod_missing\W}
        # p "not skipping"
        skipping = false
        next
      end

      skipping or filtered <<  v

    end

    # pp "filt", filtered
    # pp args[0]
    filtered
    # super args[0]
  end

end