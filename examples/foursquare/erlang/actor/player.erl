-module(player).
-export([init/1]).

-record(player,{ name, round, opponents }).

init( Name ) ->
    loop( #player{ name = Name } ).

loop( State ) ->
    receive

        { round, Opponents, Round } ->
            Round ! self(),
            NewState = State#player{ round = Round, opponents = Opponents };

        { name, From } ->
            NewState = State,
            From ! { self(), State#player.name };

        serve ->
            NewState = State,
            [ Opponent| _ ] = State#player.opponents,
            Opponent ! { name, self() },
            receive
                { Opponent, Name } ->
                    true
            end,
            io:format( "~s serves to ~s~n", [ State#player.name,
                                              Name ] ),
            Opponent ! { volley, 1 };

        { volley, Volleys } ->
            NewState = State,
            Okay = made_serve(),
            if
                Okay ->
                    Opponent = choose(State),
                    Opponent ! { name, self() },
                    receive
                        { Opponent, Name } ->
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


made_serve() ->
    random:uniform() < 0.99.

choose(State) ->
    lists:nth( random:uniform(3),State#player.opponents ).

