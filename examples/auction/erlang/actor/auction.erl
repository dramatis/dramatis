-module(auction).

-export([new/3]).
-export([inquire/1]).
-export([winner/1]).
-export([max_bid/1]).
-export([offer/2]).

-record(state,
        { seller,
          min_bid,
          closing,
          bid_increment,
          max_bid,
          max_bidder,
          winner_callbacks = [] }).

new(Seller,MinBid,Closing) ->
    spawn( fun() -> open( #state{seller = Seller,
                                 min_bid = MinBid,
                                 closing = Closing,
                                 bid_increment = 10,
                                 max_bid = MinBid - 10} ) end ).

inquire(Auction) ->
    Auction ! { inquire, self() },
    receive
        Result -> Result
    end.

winner(Auction) ->
    Auction ! { winner, self() },
    receive
        Result -> Result
    end.

max_bid(Auction) ->
    Auction ! { max_bid, self() },
    receive
        Result -> Result
    end.

offer(Auction, Current) ->
    Auction ! { offer, Current, self() }.

open(State) ->
    {_,Now,_} = now(),
    Timeout = ( State#state.closing - Now ) * 1000,
    receive
        { inquire, From } ->
            From ! { State#state.max_bid, State#state.closing },
            NewState = State;
        { offer, Bid, From } ->
            if
                Bid >= State#state.max_bid + State#state.bid_increment ->
                    if
                        State#state.max_bid >= State#state.min_bid ->
                            State#state.max_bidder ! { beaten_offer, Bid };
                        true -> true
                    end,
                    NewMaxBid = Bid,
                    NewMaxBidder = From,
                    From ! best_offer;
                true ->
                    From ! { beaten_offer, State#state.max_bid },
                    NewMaxBid = State#state.max_bid,
                    NewMaxBidder = State#state.max_bidder
            end,
            NewState = State#state{ max_bid = NewMaxBid,
                                    max_bidder = NewMaxBidder }
    after Timeout ->
            NewState = State,
            over(State)
    end,
    open(NewState).


over(State) ->
    if
        State#state.max_bid > State#state.min_bid ->
            seller:winner( State#state.seller,
                           State#state.max_bidder ),
            State#state.max_bidder !
                { auction_concluded,
                  State#state.seller,
                  State#state.max_bidder };
        true ->
            seller:failed( State#state.seller )
    end,
    receive
        {offer,_Bid,Client} ->
            Client ! auction_over;
        { winner, From } ->
            if
                State#state.max_bid > State#state.min_bid ->
                    From ! State#state.max_bidder;
                true ->
                    From ! undefined
            end;
        { max_bid, From } ->
            From ! { State#state.max_bid };
        Any ->
            exit(not_understood,Any)
    after 3000 ->
            exit(ok)
    end,
    over(State).
