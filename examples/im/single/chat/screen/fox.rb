module Chat; end
module Chat::Screen; end

require 'dramatis/actor'
require 'rubygems'
require 'fox16'

module Chat::Screen::Fox

  def self.new *args
    Server.new( *args )
  end

  class Server

    Dramatis::Actor::acts_as self

    attr_reader :fox

    def initialize
      actor.always :fox, true
      @fox = Fox::FXApp.new
      @fox.create
      ( Dramatis::Actor::Name( Runtime.new ).continue nil ).run @fox
    end

    def new *args
      Window.new( actor.name, *args )
    end

    class Runtime
      Dramatis::Actor::acts_as self
      def run fox
        fox.run
      end
    end

  end

  class Window

    Dramatis::Actor::acts_as self

    def initialize server, client, options
      
      @server = server
      @client = client
      @options = options

      @window = Fox::FXMainWindow.new server.fox, @options[:title]
      @window.connect Fox::SEL_CLOSE do; close end

      @frame = Fox::FXVerticalFrame.new @window
          
      @text = Fox::FXText.new @frame
      @text.editable = false
      @text.visibleColumns = 80
      @text.visibleRows = 20

      @input = Fox::FXTextField.new @frame, 80
      @input.text = @options[:prompt]

      @input.connect Fox::SEL_COMMAND do |sender,selector,data|
        # NB: this is a call back from Fox
        # That means this block does not hold the actor lock
        # It is (probably?) an unmanged thread
        # It's not clear how reliable this is or how it is
        # combining Ruby and Fox event loops
        begin
          # raise "hell"
        rescue Exception => e
          pp e.backtrace
        end
        @input.text = @options[:prompt]
        if data.index( @options[:prompt] ) == 0
          data = data[ @options[:prompt].length..-1 ]
        end
        Dramatis::Actor::Name( @client ).continue( nil ) >> data
      end

      @window.create
      @window.show

    end

    def << string
      @text.appendText string
    end

    def close
      @client.close
    end

  end

end
