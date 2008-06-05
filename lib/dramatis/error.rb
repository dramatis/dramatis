module Dramatis; end

# The base class of all non-internal dramatis exceptions.

class Dramatis::Error < StandardError; end

# Raised when an attempt is made to create an interface object on an
# object that does not define an interface class.

class Dramatis::Error::Interface < StandardError; end

# Raised when an attempt is made to bind an already-bound actor.

class Dramatis::Error::Bind < Dramatis::Error; end

require 'dramatis/error/uncaught'

class Dramatis::Error < StandardError

  def self._traceback exception, _next = nil
    tb = exception.instance_variable_get :@_dramatis_traceback
    if !tb
      tb = exception.instance_variable_set( :@_dramatis_traceback,
                                            Traceback.new( _next ) )
    end
    tb
  end

end

class Dramatis::Error::Traceback_

  def initialize _next
  end

end

class Exception

  alias _dramatis_set_backtrace set_backtrace 
  alias _dramatis_backtrace backtrace 

  def set_backtrace arg
    # p "set back #{self}"
    _dramatis_set_backtrace arg
  end

  def backtrace
    # p "back #{self}"
    if instance_variable_defined? :@_dramatis_raw_backtrace
      filter @_dramatis_raw_backtrace
    else
      _dramatis_backtrace
    end
  end

  def _dramatis_reraise
    # p "reraise"
    if @_dramatis_raw_backtrace
      @_dramatis_raw_backtrace = backtrace + filter( caller )
    else
      @_dramatis_raw_backtrace = filter( backtrace ) + filter( caller )
    end
  end

  def filter array

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
