from __future__ import absolute_import

import dramatis

class Client( dramatis.Actor ):

    def __init__( self, screen, server, password, group, user ):
        self._user = user

        self._window = screen( self.actor.name,
                                title = user,
                                prompt = "%s > " % self._user )
                               
        self._window << "connecting to server ... "

        try:
            self._server = server.connect( password )
        except Exception, e:
            self._window << "\nfailed to connect to server: %s\n" % e
            return

        self._window << "connected to server %s\n" % self._server

        self._window << "logging in ... "

        try:
            self._group = self._server.login( group, user, self.actor.name )
            self._window << "done\n"
        except Exception, e:
            self._window << "\nlogin failed: %s\n" % e

"""

  def << opts
    self._window << "#{opts[:user]}self._#{opts[:client]} #{opts[:string]}\n"
  end

  def >> string
    self._group << { :user => self._user, :client => self, :string => string }
  end

  def close
    self._group.logout actor.name
    0
  end

end

"""
