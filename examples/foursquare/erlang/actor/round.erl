-module(round).
-export([init/1]).
-record(round,{players,from}).

init(Players) ->
    lists:foreach( fun(Player) ->
                           Opponents = [ Opponent || Opponent <- Players,
                                                     Opponent /= Player ],
                           Player ! { round, Opponents, self() },
                           receive Player -> true end
                   end,
                   Players ),
    loop( #round{ players = Players } ).

loop( State ) ->
    receive
        { play, From } ->
            Players = State#round.players,
            Server = lists:last(Players),
            Server ! serve,
            NewState = State#round{ from = From };
        { failed, Loser, Volleys } ->
            NewState = State,
            From = State#round.from,
            From ! { result, Loser, Volleys };
        Any ->
            NewState = State,
            io:format( "round didn't expect ~p~n", [Any] ),
            exit(failed)
    end,
    loop(NewState).

