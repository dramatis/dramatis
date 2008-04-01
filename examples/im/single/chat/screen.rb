module Chat; end

require 'chat/screen/fox'

module Chat::Screen

  def self.new *args
    Chat::Screen::Fox::Server.new( *args )
  end

end
