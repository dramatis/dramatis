#!/usr/bin/env python

# cf. Scala by Example, Chapter 3

import time
import random
import inspect
import sys
import os.path

sys.path[0:0] = [ os.path.join( os.path.dirname( inspect.getabsfile( inspect.currentframe() ) ), '..', 'lib' ) ]

from logging import warning

import dramatis

class Auction ( dramatis.Actor ):
    def __init__(self, seller, min_bid, closing):
        self._seller = seller
        self._min_bid = min_bid
        self._closing = closing

        self._time_to_shutdown = 3000
        self._bid_increment = 10
        self._max_bid = self._min_bid - self._bid_increment
        self._max_bidder = None

    def inquire(self):
        return [ self._max_bid, self._closing ]

    def offer(self, bid, bidder):
        if bid >= self._max_bid + self._bid_increment:
            if self._max_bid >= self._min_bid:
                dramatis.release( self._max_bidder ).beaten_offer( bid )
            self._max_bid = bid
            self._max_bidder = bidder
            return [ "best_offer", None ]
        else:
            return [ "beaten_offer", self._max_bid ]

MIN_BID = 100
CLOSING = time.time() + 4

seller = dramatis.Actor( object() )
auction = Auction( seller, MIN_BID, CLOSING )

class Client ( dramatis.Actor ):
    def __init__(self, id, increment, top, auction):
        self._id = id
        self._increment = increment
        self._top = top
        self._auction = auction
        self._current = 0
        self.log( "started" )
        self._max = auction.inquire()[0]
        self.log( "status " + str(self._max) )
        self.bid()

    def bid(self):
        if self._max > self._top:
            self.log("too high for me")
        elif ( self._current < self._max ):
            self._current = self._max + self._increment
            time.sleep( ( 1 + random.randint( 0, 1000 ) )/1000.0 )
            answer, max_bid = self._auction.offer( self._current,
                                                   self.actor.name )
            if answer == "best_offer": self.log("best offer: " + str(self._current))
            elif answer == "beaten_offer": self.beaten_offer( max_bid )

    def beaten_offer(self, max_bid):
        self.log("beaten offer: " + str(max_bid))
        self._max = max_bid
        dramatis.release( self.actor.name ).bid()

    def log(self,string):
        print "client", self._id, ": ", string

Client( 1, 20, 200, auction )
Client( 2, 10, 300, auction )
