module Dramatis; end
class Dramatis::Runtime; end

class Dramatis::Runtime::Gate

  def initialize block
    @filter = block
  end

  def set block
    @filter = block
  end

  def accepts? task
    @filter.call task
  end

end
