module Dramatis; end

class Dramatis::Error < StandardError; end
class Dramatis::Deadlock < Dramatis::Error; end
class Dramatis::BindError < Dramatis::Error; end
class Dramatis::Internal < Dramatis::Error; end

require 'dramatis/future/proxy'

module Dramatis

  def dramatis *args, &block
    Dramatis::Actor::Name::Proxy.new( *args, &block )
  end

  module_function :dramatis

  def cast name
    dramatis( name ).continue nil
  end

  module_function :cast

  def future name
    dramatis( name ).future
  end

  module_function :future

  def Future *args, &block
    Dramatis::Future::Proxy.new( *args, &block )
  end

  module_function :Future

end
