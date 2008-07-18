-module(play).
-export([play/0]).

play() ->
    Players = [ spawn_link( fun() -> player:init( "John" ) end ),
                spawn_link( fun() -> player:init( "Paul" ) end ),
                spawn_link( fun() -> player:init( "George" ) end ),
                spawn_link( fun() -> player:init( "Ringo" ) end ) ],
    Round = spawn_link( fun() -> round:init( Players ) end ),
    Round ! { play, self() },
    receive
        { Round, Loser, Volleys } ->
            Loser ! { name, self() },
            receive
                { Loser, Name } -> 
                    io:format( "~s lost after ~p volleys~n",
                               [ Name, Volleys ] )
            end
    end.
    


