module Chat; end

require 'dramatis/actor'

class Chat::Client

  Dramatis::Actor.acts_as self

  def initialize screen, server, password, group, user

    @user = user

    @window = screen.new actor.name, :title => user, :prompt => "#{@user} > "

    @window << "connecting to server ... "

    begin
      @server = server.connect password
    rescue Exception => e
      @window << "\nfailed to connect to server: #{e}\n"
      return
    end

    @window << "connected to server #{@server}\n"

    @window << "logging in ... "

    begin
      @group = @server.login group, user, actor.name
      @window << "done\n"
    rescue Exception => e
      @window << "\nlogin failed: #{e}\n"
    end

  end

  def << opts
    @window << "#{opts[:user]}@#{opts[:client]} #{opts[:string]}\n"
  end

  def >> string
    @group << { :user => @user, :client => self, :string => string }
  end

  def close
    @group.logout actor.name
    0
  end

end
