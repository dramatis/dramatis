#!/usr/bin/env ruby

$:.push File.join( File.dirname(__FILE__), "..", "..", "lib" )

require 'dramatis/actor'

module FourSquare; end

class FourSquare::Player
  
  include Dramatis::Actor

  def initialize name
    actor.enable_call_threading
    @name = name
  end

  def to_s
    @name
  end

  def round round, opponents
    @round = round
    @opponents = opponents
  end

  def serve ball
    # We ignore the possiblity of service faults
    puts "#{self} serves to #{@opponents[0]}"
    @opponents[0].volley ball
  end

  def volley ball
    # We ignore serve do-overs
    if made_save
      opponent = choose
      puts "#{self} hits to #{opponent}"
      opponent.volley ball
    else
      @round.failed self
    end
  end

private

  def made_save
    rand < 0.999
  end  

  def choose
    @opponents[ rand(3) ]
  end  

end

class FourSquare::Ball; end

class FourSquare::Round

  include Dramatis::Actor

  attr_reader :players, :loser
  
  def initialize players
    actor.enable_call_threading
    @players = players
    @players.each do |player|
      player.round self, @players.select { |opponent| opponent != player }
    end
  end

  def play
    @players[3].serve Ball.new
  end

  def failed loser
    @loser = loser
  end

end

include FourSquare

players = [ Player.new( "John" ),
            Player.new( "Paul" ),
            Player.new( "George" ),
            Player.new( "Ringo" ) ]

round = Round.new players
round.play
puts "#{round.loser} lost"
