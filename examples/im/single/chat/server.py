from __future__ import absolute_import

import dramatis

class Server( dramatis.Actor ):

    def __init__( self, password ):
        super(Server,self).__init__()
        self._password = password
        self._groups = {}

"""
  def connect password
    if password == self._password
      Connection.new actor.name
    else
      raise "bad password"
    end
  end

  def login group, user, client
    if !actor = self._groups[group]
      actor = self._groups[group] = Group.new( group, user, client )
    else
      actor.login user, client
    end
    actor
  end

  class Connection
    include Dramatis::Actor
    def initialize server
      self._server = server
    end

    def login group, user, client
      self._server.login group, user, client
    end
  end

  class Group
    include Dramatis::Actor

    def initialize name, user, client
      self._name = name
      self._clients = { client => user }
      self._cast = interface( actor.name ).continue nil

      self._cast << { :user => user, :client => client, :string => "I'm starting the group" }
    end

    def login user, client
      self._clients[client] = user
      self._cast << { :user => user, :client => client, :string => "I'm joining the group" }
    end

    def logout client
      user = self._clients[client]
      self._clients.delete client
      self._cast << { :user => user, :client => client, :string => "I'm leaving the group" }
    end

    def << opts
      self._clients.each_key do |client|
        ( interface( client ).continue nil )  << opts
      end
    end

  end

end

"""
