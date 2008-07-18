-module(play).
-export([play/0]).

play() ->
    Players = { player:create( "John" ),
                player:create( "Paul" ),
                player:create( "George" ),
                player:create( "Ringo" ) },
    Before = round:create( Players ),
    After = round:play( Before ),
    io:format( "~s lost after ~p volleys~n",
               [ player:name( round:loser( After ) ),
                 round:volleys( After ) ] ).
    


