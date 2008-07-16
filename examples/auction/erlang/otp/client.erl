-module(client).
-behaviour(gen_server).

-export([new/4]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3]).
-export([terminate/2]).

-record(state,
        { name, increment, top, auction, current, max, winner }).

new( Name, Increment, Top, Auction ) ->
    {ok,Pid} = gen_server:start_link(?MODULE,
                                     [Name, Increment, Top, Auction],
                                     []),
    Pid.

init([ Name, Increment, Top, Auction ]) ->
    {Max,_} = auction:inquire( Auction ),
    {ok,#state{name = Name,
               increment = Increment,
               top = Top,
               auction = Auction,
               current = 0,
               max = Max}}.

handle_call(_,_,_) -> fail.
handle_cast(_,_) -> fail.
handle_info(_,_) -> fail.
code_change(_,_,_) -> fail.
terminate(_,_) -> fail.
