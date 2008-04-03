module Chat; end

require 'chat/screen/fox'

module Chat::Screen

  def self.new *args
    Chat::Screen::Fox.new( *args )
  end

end
