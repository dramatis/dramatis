#!/usr/bin/env ruby

# cf. Scala by Example, Chapter 3

# Note: requires dramatis > 0.1.1

$:.push File.join( File.dirname(__FILE__), "..", "..", "lib" )

require 'dramatis/actor'
require 'dramatis/actor/behavior'

module Auction

  def self.new *args
    Open.new( *args )
  end

  class Open
    include Dramatis::Actor
    def initialize seller, min_bid, closing
      @seller = seller
      @min_bid = min_bid
      @closing = closing

      @bid_increment = 10
      @max_bid = @min_bid - @bid_increment
      @max_bidder = nil

      actor.refuse :winner

      release( actor.name ).close
    end

    def close
      actor.yield @closing - Time::now

      if @max_bid > @min_bid
        release( @seller ).winner @max_bidder
        release( @max_bidder ).winner @seller
        actor.become Over.new( @max_bidder, @max_bid )
      else
        release( @seller ).failed @max_bid
        actor.become Over.new( nil, @max_bid )
      end
    end

    def inquire
      [ @max_bid, @closing ]
    end

    def offer bid, bidder
      if bid >= @max_bid + @bid_increment
        if @max_bid >= @min_bid
          release( @max_bidder ).beaten_offer bid
        end
        @max_bid = bid
        @max_bidder = bidder
        :best_offer
      else
        [ :beaten_offer, @max_bid ]
      end
    end
  end

  class Over
    include Dramatis::Actor::Behavior
    attr_reader :winner, :max_bid
    def initialize winner, max_bid
      @winner = winner
      @max_bid = max_bid
    end
    def dramatis_bound
      actor.accept :winner
    end
    def inquire *args
      [ @max_bid, 0 ]
    end
    def offer *args
      :auction_over
    end
  end

end

class Seller
  include Dramatis::Actor
  def winner winner
  end
  def failed highest_bid
  end
end

class Client
  include Dramatis::Actor
  attr_reader :name
  def initialize name, increment, top, auction
    @name = name
    @increment = increment
    @top = top
    @auction = auction
    @current = 0
    log "started"
    @max = auction.inquire[0]
    log "status #{@max}"
    release( actor.name ).bid
  end
  def bid
    # log("bid chance: current: #{@current} max: #{@max} top: #{@top}")
    if @max >= @top
      log("too high for me")
    elsif ( @current <= @max )
      @current = @max + @increment
      sleep( ( 1 + rand( 1000 ) )/1000.0 )
      # log("i bid #{@current}")
      answer, max_bid = @auction.offer @current, actor.name
      case answer
      when :best_offer; log("best offer: #{@current}")
      when :beaten_offer; beaten_offer max_bid
      when :auction_over; log("auction over, oh well")
      end
    else
      raise "should not get here ..."
    end
  end
  def beaten_offer max_bid
    log("beaten offer: #{max_bid}")
    @max = max_bid
    release( actor.name ).bid
  end
  def winner seller
    log "I won!"
  end
  def log string
    puts "client #{@name}: #{string}"
  end
end

seller = Seller.new
auction = Auction.new seller, 100, Time::now + 4
Client.new "Tom", 20, 200, auction
Client.new "Dick", 10, 300, auction
Client.new "Harry", 20, 400, auction

puts "Notice: client #{auction.winner.name} won the first auction with a bid of #{auction.max_bid}"
