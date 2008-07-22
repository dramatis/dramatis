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
        { result, Loser, Volleys } ->
            Loser ! { name, self() },
            receive
                { result, Name } ->  true
            end,
            io:format( "~s lost after ~p volleys~n",
                       [ Name, Volleys ] )
    end.
    


