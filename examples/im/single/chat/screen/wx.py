from __future__ import absolute_import

import dramatis
import wx

class Screen(object):

    def __new__( cls ):
        return Screen.Server()

    class Server( dramatis.Actor ):

        def __init__( self ):
            self.actor.always( "wx", True )
            self._wx = Screen.Server.App()
            ( dramatis.release( self._wx ) ).run()

        def __call__( self, *args, **kwds ):
            return Screen.Window( self.actor.name, *args, **kwds )

        class App ( dramatis.Actor, wx.App ):

            def run( self ):
                self.MainLoop()

            def OnInit( self ):
                print "hi!"
                hidden = wx.Frame( None, -1, "hello" )
                hidden.Show()
                self.SetTopWindow(hidden)
                return True

    class Window ( dramatis.Actor ):

        def __init__( self, server, client, **options ):
            self._server = server
            self._client = client
            self._options = options

            self._window = wx.Frame( None, wx.ID_ANY )
            self._window.Show()

        def __lshift__( self, string ):
            return self._text.appendText( string )


"""
      self._window = Fox::FXMainWindow.new server.fox, self._options[:title]
      self._window.connect Fox::SEL_CLOSE do; close end

      self._frame = Fox::FXVerticalFrame.new self._window
          
      self._text = Fox::FXText.new self._frame
      self._text.editable = false
      self._text.visibleColumns = 80
      self._text.visibleRows = 20

      self._input = Fox::FXTextField.new self._frame, 80
      self._input.text = self._options[:prompt]

      self._input.connect Fox::SEL_COMMAND do |sender,selector,data|
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
        self._input.text = self._options[:prompt]
        if data.index( self._options[:prompt] ) == 0
          data = data[ self._options[:prompt].length..-1 ]
        end
        interface( self._client ).continue( nil ) >> data
      end

      self._window.create
      self._window.show

    end

    def << string
      self._text.appendText string
    end

    def close
      self._client.close
    end

  end

end
"""
