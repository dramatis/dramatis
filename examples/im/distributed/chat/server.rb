module Chat; end

require 'dramatis/actor'

class Chat::Server

  include Dramatis::Actor

  def initialize password
    @password = password
    @groups = {}
  end

  def connect password
    if password == @password
      Connection.new actor.name
    else
      raise "bad password"
    end
  end

  def login group, user, client
    if !actor = @groups[group]
      actor = @groups[group] = Group.new( group, user, client )
    else
      actor.login user, client
    end
    actor
  end

  class Connection
    include Dramatis::Actor
    def initialize server
      @server = server
    end

    def login group, user, client
      @server.login group, user, client
    end
  end

  class Group
    include Dramatis::Actor

    def initialize name, user, client
      @name = name
      @clients = { client => user }
      @cast = interface( actor.name ).continue nil

      @cast << { :user => user, :client => client, :string => "I'm starting the group" }
    end

    def login user, client
      @clients[client] = user
      @cast << { :user => user, :client => client, :string => "I'm joining the group" }
    end

    def logout client
      user = @clients[client]
      @clients.delete client
      @cast << { :user => user, :client => client, :string => "I'm leaving the group" }
    end

    def << opts
      @clients.each_key do |client|
        ( interface( client ).continue nil )  << opts
      end
    end

  end

end
