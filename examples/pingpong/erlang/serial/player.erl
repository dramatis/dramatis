-module(player).
-export([create/1,volley/3]).

create( Name ) -> { Name }.

volley(Player, Volleys, Opponent) ->
    { Name } = Player,
    if
        Volleys == 0 ->
            io:format("~s: done~n",[Name]);
        true ->
            if
                ( Volleys rem 500 == 0 ) or ( Volleys rem 500 == 1 ) ->
                    io:format("~s: volley ~p~n", [Name, Volleys]);
                true -> true
            end,
            volley(Opponent,Volleys-1,Player)
    end.
