#!/usr/bin/env ruby

class PingPong

  def initialize name
    @name = name
  end

  def pingpong count, partner
    if count == 0
      puts "#{@name}: done"
    else
      if count % 500 == 0 || count % 500 == 1
        puts "#{@name}: pingpong #{count}"
      end
      partner.pingpong count-1, self
      # sleep 0.001
    end
  end

end

ping = PingPong.new "ping"
pong = PingPong.new "pong"

ping.pingpong ARGV[0].to_i, pong
