-module(play).
-export([play/0]).

play() ->
    Ping = player:create( "ping" ),
    Pong = player:create( "pong" ),
    player:volley( Ping, 1000, Pong ).
    


