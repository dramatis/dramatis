module Dramatis; end
class Dramatis::Future; end

# A Dramatis::Future::Interface object provides the ability to
# observe and access the semantics of a future. It is typically
# created via Dramatis.interface.

class Dramatis::Future::Interface

  # Returns the native value of the future. If the value of the future
  # is not yet available, the method blocks (with rpc gating
  # semantics) until it is.
  #
  # In many cases, this method is not necessary since the
  # method_missing method on the future will catch most attempts to
  # accesses the value. This method may be necessary in corner cases,
  # for example when usingconditionals, conversions, and
  # metaprogramming.

  def value
    @future.instance_eval do
      @continuation.value
    end
  end

  # Returns true if the future may be evaluated without
  # blocking. Returns false if the value is not yet available.
  #
  # Once a future is ready it cannot become unready, so once ready? returns
  # true, it will always be true and value access will never block.

  def ready?
    @future.instance_eval do
      @continuation.ready?
    end
  end

  def initialize *args, &block #:nodoc:
    @future = args.shift
    @args = args
    @block = block
  end

end

