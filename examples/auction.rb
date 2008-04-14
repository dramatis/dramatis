#!/usr/bin/env ruby

# cf. Scala by Example, Chapter 3

$:.push File.join( File.dirname(__FILE__), "..", "lib" )

require 'dramatis/actor'

class Auction
  include Dramatis::Actor
  def initialize seller, min_bid, closing
    @seller = seller
    @min_bid = min_bid
    @closing = closing

    @time_to_shutdown = 3000
    @bid_increment = 10
    @max_bid = @min_bid - @bid_increment
    @max_bidder = nil

    actor.timeout @closing - Time::now, :timeout
  end

  def inquire
    [ @max_bid, @closing ]
  end

  def offer bid, bidder
    if bid >= @max_bid + @bid_increment
      if @max_bid >= @min_bid
        cast( @max_bidder ).beaten_offer bid
      end
      @max_bid = bid
      @max_bidder = bidder
      :best_offer
    else
      [ :beaten_offer, @max_bid ]
    end
  end

  def timeout
    actor.timeout( @time_to_shtudown  ) { exit }
    actor.become Over.new
  end
end

MIN_BID = 100
CLOSING = Time::now + 4

seller = Dramatis::Actor.new Object.new
auction = Auction.new seller, MIN_BID, CLOSING

class Client
  include Dramatis::Actor
  def initialize id, increment, top, auction
    @id = id
    @increment = increment
    @top = top
    @auction = auction
    @current = 0
    log "started"
    @max = auction.inquire[0]
    log "status #{@max}"
    bid
  end
  def bid
    if @max > @top
      log("too high for me")
    elsif ( @current < @max )
      @current = @max + @increment
      sleep( ( 1 + rand( 1000 ) )/1000.0 )
      answer, max_bid = @auction.offer @current, actor.name
      case answer
      when :best_offer; log("best offer: #{@current}")
      when :beaten_offer; beaten_offer max_bid
      end
    end
  end
  def beaten_offer max_bid
    log("beaten offer: #{max_bid}")
    @max = max_bid
    cast( actor.name ).bid
  end
  def log string
    puts "client #{@id}: #{string}"
  end
end

Client.new 1, 20, 200, auction
Client.new 2, 10, 300, auction
