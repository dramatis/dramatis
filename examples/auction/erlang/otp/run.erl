-module(run).
-export([run/0]).

run() ->
    Seller = seller:new(),
    {_,Seconds,_} = now(),
    Auction = auction:new( Seller, 100, Seconds + 4 ),
    C1 = client:new( "1a", 20, 200, Auction ),
    io:format( "? ~p~n", [ C1 ] ).
    io:format( "? ~p~n", [ client:namey(C1) ] ).

foo() ->
    % somebody gives up
    Seller = seller:new(),
    {_,Seconds,_} = now(),
    Auction = auction:new( Seller, 100, Seconds + 4 ),
    C1 = client:new( "1a", 20, 200, Auction ),
    client:new( "1a", 10, 300, Auction ),
    io:format( "? ~p~n", [ client:namey(C1) ] ),
    io:format( "Notice: client ~p won the first auction with a bid of ~p\n",
               [ client:name( auction:winner( Auction ) ),
                 auction:max_bid( Auction ) ] ).


