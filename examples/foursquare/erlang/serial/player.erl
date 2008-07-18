-module(player).
-export([create/1,serve/2,name/1]).

-record(player,{ name }).

create( Name ) -> #player{ name = Name }.

name( Player ) -> Player#player.name.

serve( Player, Round ) ->
    Opponent = first( round:players( Round ) ),
    Name = player:name( Opponent ),
    io:format( "~s serves to ~s~n", [ Player#player.name,
                                      Name ] ),
    volley( Opponent, Round, 1 ).

volley(Player, Round, Volleys) ->
    Okay = made_serve(),
    if
        Okay ->
            Opponent = choose(Player,Round),
            io:format( "~s hits to ~s~n", [ Player#player.name,
                                            Opponent#player.name ] ),
            volley(Opponent, Round, Volleys+1);
        true ->
            round:failed( Round, Player, Volleys )
    end.

made_serve() ->
    random:uniform() < 0.999.

choose(Player,Round) ->
    OpponentIndex = random:uniform(3),
    Opponent = nth( OpponentIndex, round:players(Round) ),
    if
        Opponent == Player ->
            Index = OpponentIndex + 1;
        true ->
            Index = OpponentIndex
    end,
    nth( Index, round:players(Round) ).

nth( Index, Players ) ->
    case Index of
        1 -> first( Players );
        2 -> second( Players );
        3 -> third( Players );
        4 -> fourth( Players )
    end.

first( Players ) ->
    { Player, _, _, _ } = Players,
    Player.

second( Players ) ->
    { _, Player, _, _ } = Players,
    Player.

third( Players ) ->
    { _, _, Player, _ } = Players,
    Player.

fourth( Players ) ->
    { _, _, _, Player } = Players,
    Player.

