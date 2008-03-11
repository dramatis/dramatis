module Dramatis; end
module Dramatis::Shoes; end

require 'dramatis/shoes/runtime'

module Dramatis::Shoes
  
  def self.runtime
    @runtime ||= Dramatis::Shoes::Runtime.new
  end
  
end


