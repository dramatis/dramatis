#!/usr/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "..", "lib" )

require 'dramatis/actor'

class PingPong

  include Dramatis::Actor

  def initialize name
    @name = name
  end

  def pingpong count, partner
    if count == 0
      puts "#{@name}: done"
    else
      if count % 500 == 0 or count % 500 == 1
        puts "#{@name}: pingpong #{count}"
      end
      release( partner ).pingpong count-1, self
    end
    # sleep 0.001
  end

end

ping = PingPong.new "ping"
pong = PingPong.new "pong"

ping.pingpong ARGV[0].to_i, pong

