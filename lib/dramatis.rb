module Dramatis; end

class Dramatis::Error < StandardError; end
class Dramatis::Deadlock < Dramatis::Error; end
class Dramatis::BindError < Dramatis::Error; end
class Dramatis::Internal < Dramatis::Error; end

require 'dramatis/future/proxy'

module Dramatis

  def self.Future *args, &block
    Dramatis::Future::Proxy.new( *args, &block )
  end

end
