module Dramatis; end
module Dramatis::Runtime; end
class Dramatis::Runtime::Actor; end
module Dramatis::Runtime::Actor::Name; end

class Dramatis::Runtime::Actor::Name::Proxy

  def initialize *args
    @name = args[0]
    @opts = args[1]
  end
  
  def continue options = {}, &block
    @continuation = [ options, block ]
    @name
  end

  def bind object
    unbox :bind, object
    @name
  end

  private

  def unbox *args
    raise "hell" if @continuation and @continuation[0] == nil
    @name.instance_eval do
      @binding.send *args
    end
  end

end
