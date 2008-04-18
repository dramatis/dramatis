#!/usr/bin/env ruby

# cf. http://lamp.epfl.ch/~phaller/doc/ActorsTutorial.html

$:.push File.join( File.dirname(__FILE__), "..", "..", "lib" )

require 'dramatis/actor'

class Ping
  include Dramatis::Actor
  def initialize times, pong
    @pings_left = times
    cast( pong ).ping actor.name 
  end
  def pong caller
    if @pings_left % 1000 == 0
      puts "Ping: pong"
    end
    if @pings_left > 0
      @pings_left -= 1
      cast( caller ).ping actor.name
    end
  end
end

class Pong
  include Dramatis::Actor
  def initialize
    @pong_count = 0
  end
  def ping caller
    if @pong_count % 1000 == 0
      puts "Pong: ping #{@pong_count}"
    end
    @pong_count += 1
    cast( caller ).pong actor.name
  end
end

pong = Pong.new
ping = Ping.new 10000, pong
