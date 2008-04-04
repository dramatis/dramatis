#!/usr/bin/env ruby

# cf. http://www.softwaresecretweapons.com/jspwiki/doug-lea-is-a-grandfather-of-all-scala-actors

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor'

class Ping
  Dramatis::Actor::acts_as self
  def initialize times, pong
    @pings_left = times
    Dramatis::Actor::cast( pong ).ping actor.name 
  end
  def pong sender
    if @pings_left % 1000 == 0
      puts "Ping: pong"
    end
    if @pings_left > 0
      @pings_left -= 1
      Dramatis::Actor::cast( sender ).ping actor.name
    end
  end
end

class Pong
  Dramatis::Actor::acts_as self
  def initialize
    @pong_count = 0
  end
  def ping sender
    if @pong_count % 1000 == 0
      puts "Pong: ping #{@pong_count}"
    end
    @pong_count += 1
    Dramatis::Actor::cast( sender ).pong actor.name
  end
end

pong = Pong.new
ping = Ping.new 10000, pong
