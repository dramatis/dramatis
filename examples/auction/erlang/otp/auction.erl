-module(auction).
-behaviour(gen_server).

-export([new/3]).
-export([inquire/1]).
-export([winner/1]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3]).
-export([terminate/2]).

-record(state,
        { seller,
          min_bid,
          closing,
          bid_increment,
          max_bid,
          max_bidder,
          winner_callbacks = [] }).

new(Seller,MinBid,Closing) ->
    {ok,Pid} = gen_server:start_link(?MODULE,[Seller,MinBid,Closing],[]),
    Pid.

inquire(Auction) ->
    gen_server:call(Auction,{inquire}).

winner(Auction) ->
    gen_server:call(Auction,{winner}).

init([Seller,MinBid,Closing]) ->
    {ok,#state{seller = Seller,
               min_bid = MinBid,
               closing = Closing,
               bid_increment = 10,
               max_bid = MinBid - 10 }}.

handle_call({inquire},_From,State) ->
    {reply,{State#state.max_bid,State#state.closing},State};
handle_call({winner},From,State) ->
    case State#state.max_bid of
        undefined ->
            {noreply, State#state{ winner_callbacks = 
                                   [ From | State#state.winner_callbacks ]} }
    {reply,{State#state.max_bid,State#state.closing},State};
handle_call(_,_,_) -> fail.

handle_cast(_,_) -> fail.
handle_info(_,_) -> fail.
code_change(_,_,_) -> fail.
terminate(_,_) -> fail.
