module Chat; end
module Chat::Screen; end

require 'dramatis/actor'
require 'rubygems'

module Chat::Screen::WX

  def self.new *args
    Server.new( *args )
  end

  class Server
  end

end
