-module(run).
-export([run/0]).

run() ->
    % somebody gives up
    Seller = seller:new(),
    {_,Seconds,_} = now(),
    Auction = auction:new( Seller, 100, Seconds + 4 ),
    client:new( "1a", 20, 200, Auction ),
    client:new( "1b", 10, 300, Auction ),
    io:format( "Notice: client ~p won the first auction with a bid of ~p\n",
               [ auction:winner( Auction ),
                 auction:max_bid( Auction ) ] ).


