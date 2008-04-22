module Chat; end
module Chat::Screen; end

require 'dramatis/actor'
require 'rubygems'
require 'wx'

module Chat::Screen::WX

  def self.new *args
    Server.new( *args )
  end

  class Server
    include Dramatis::Actor
    attr_reader :wx
    def initialize
      actor.always :wx, true
      @wx = App.new
      release( @wx ).run
    end

    def new *args
      @wx ||= App.new
      Window.new( actor.name, *args )
    end

    class App < Wx::App
      include Dramatis::Actor
      def run
        main_loop
      end
      def on_init
        t = Wx::Timer.new(self, 55)
        evt_timer(55) do
          p = Thread.current.priority
          Thread.current.priority = p - 10
          Thread.pass
          Thread.current.priority = p
        end
        t.start(100)

        hidden = Wx::Frame.new nil, -1, "hello"
        hidden.show
      end
    end
  end

  class Window

    def initialize server, client, options
      @window = Wx::Frame.new nil, Wx::ID_ANY
      @window.show
      # layout = Wx::BoxSizer.new(Wx::VERTICAL)
      # set_sizer(layout)
    end

    def << string
    end

  end

end
