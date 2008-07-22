-module(player).
-export([init/1]).

-record(player,{ name, round, opponents }).

init( Name ) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    loop( #player{ name = Name } ).

loop( State ) ->
    receive
        { name, From } ->
            NewState = State,
            From ! { result, State#player.name };

        { round, Opponents, Round } ->
            Round ! self(),
            NewState = State#player{ round = Round, opponents = Opponents };

        serve ->
            NewState = State,
            [ Opponent| _ ] = State#player.opponents,
            Opponent ! { name, self() },
            receive
                { result, OpponentName } -> true
            end,
            io:format( "~s serves to ~s~n", [ State#player.name,
                                              OpponentName ] ),
            Opponent ! { volley, 1 };

        { volley, Volleys } ->
            NewState = State,
            Okay = made_save(),
            if
                Okay ->
                    Opponent = choose(State),
                    Opponent ! { name, self() },
                    receive
                        { result, Name } ->
                            true
                    end,
                    io:format( "~s hits to ~s~n", [ State#player.name,
                                                    Name ] ),
                    Opponent ! { volley, Volleys + 1 };
                true ->
                    State#player.round ! { failed, self(), Volleys }
            end;

        Any ->
            NewState = State,
            io:format( "player didn't expect ~p~n", [Any] ),
            exit(failed)
    end,
    loop(NewState).


made_save() ->
    random:uniform() < 0.9.

choose(State) ->
    lists:nth( random:uniform(3),State#player.opponents ).

