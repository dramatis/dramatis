#!/usr/bin/env ruby

module FourSquare; end

class FourSquare::Player
  
  attr_reader :name

  def initialize name
    @name = name
  end

  def join round, opponents
    @round = round
    @opponents = opponents
  end

  def serve
    # We ignore the possiblity
    # of service faults
    print "#{name} serves ",
          "to #{@opponents[0].name}\n"
    @opponents[0].volley 1
  end

  def volley volleys
    # We ignore serve do-overs
    # We ignore out of bound hits
    if made_save()
      # we ignore bad hits
      opponent = choose()
      opponent_name = opponent.name
      puts "#{name} hits to #{opponent_name}"
      opponent.volley volleys + 1
    else
      @round.failed self, volleys
    end
  end

private

  def made_save
    rand < 0.99
  end  

  def choose
    @opponents[ rand(3) ]
  end  

end

class FourSquare::Round

  attr_reader :players, :loser, :volleys
  
  def initialize players
    @players = players
    @players.each do |player|
      opponents =
        @players.select do |opponent|
          opponent != player
        end
      player.join self, opponents
    end
  end

  def play
    @players[-1].serve
  end

  def failed loser, volleys
    @loser = loser
    @volleys = volleys
  end

end

include FourSquare

players = [ Player.new( "John" ),
            Player.new( "Paul" ),
            Player.new( "George" ),
            Player.new( "Ringo" ) ]

round = Round.new players
round.play
print "#{round.loser.name} lost after ",
      "#{round.volleys} volleys\n"
