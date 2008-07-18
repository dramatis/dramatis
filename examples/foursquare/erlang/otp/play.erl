-module(play).
-export([play/0]).

play() ->
    Players = [ player:create( "John" ),
                player:create( "Paul" ),
                player:create( "George" ),
                player:create( "Ringo" ) ],
    Round = round:create( Players ),
    round:play( Round ),
    io:format( "~s lost after ~p volleys~n",
               [ player:name( round:loser( Round ) ),
                 round:volleys( Round ) ] ).
    


