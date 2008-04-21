module Dramatis; end
class Dramatis::Runtime; end

class Dramatis::Runtime::Futurex

  private

  def method_missing *args
    @continuation.value.send( *args )
  end

  def initialize continuation
    @continuation = continuation
  end

end
