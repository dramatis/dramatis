module Dramatis; end

# Dramatis::Futures are proxy objects for the values returned from
# actor method calls made with future continuations. When a method is
# called on a future, the runtime checks to see if the future has been
# evaluated and returned from the actor that executed the task. If it
# has, the method is executed on the returned value as if the proxy
# object was not there.
#
# If the task with the future continuation has not yet completed or
# the continuation task has not yet been run, the method called on
# the proxy is suspended until the reply is received. Thus, methods on
# futures sometimes but not always block. If they block, they have
# normal continuation gating semantics.
#
# Dramatis::Future has no user-callable methods (except for the
# implicit method_missing). Other future operations are available
# through the Dramatis::Future::Interface object, accessible via
# Dramatis.interface.

class Dramatis::Future

  def method_missing *args #:nodoc:
    @continuation.value.send( *args )
  end

  def initialize continuation #:nodoc:
    @continuation = continuation
  end

end

