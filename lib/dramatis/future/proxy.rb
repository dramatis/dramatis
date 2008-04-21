module Dramatis; end
class Dramatis::Future; end

class Dramatis::Future::Proxyx

  def value
    @future.instance_eval do
      @continuation.value
    end
  end

  def ready?
    @future.instance_eval do
      @continuation.ready?
    end
  end

  def initialize *args, &block
    @future = args.shift
    @args = args
    @block = block
  end

end

