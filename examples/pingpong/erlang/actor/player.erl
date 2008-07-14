-module(player).
-export([create/1,volley/3]).

create( Name ) ->
    spawn( fun() -> loop({Name}) end ).

volley(Player, Volleys, Opponent) ->
    Player ! { send_volley, Volleys, Opponent }.

loop(State) ->
    { Name } = State,
    receive
        { send_volley, Volleys, Opponent } ->
            if
                Volleys == 0 ->
                    io:format("~s: done~n",[Name]);
                true ->
                    if
                        ( Volleys rem 500 == 0 ) or ( Volleys rem 500 == 1 ) ->
                            io:format("~s: volley ~p~n", [Name, Volleys]);
                        true -> true
                    end,
                    volley(Opponent,Volleys-1,self()),
                    loop({Name})
            end
    end.
