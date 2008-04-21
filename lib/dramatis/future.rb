module Dramatis; end

class Dramatis::Future

  def method_missing *args
    @continuation.value.send( *args )
  end

  def initialize continuation
    @continuation = continuation
  end

end

