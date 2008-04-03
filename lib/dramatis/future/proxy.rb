module Dramatis; end
module Dramatis::Future; end

class Dramatis::Future::Proxy

  def initialize *args, &block
    @future = args.shift
    @args = args
    @block = block
  end

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

end

