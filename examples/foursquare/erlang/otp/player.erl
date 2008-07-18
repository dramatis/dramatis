-module(player).
-behaviour(gen_server).
-export([create/1,serve/1,volley/2,name/1,round/3]).
-export([init/1,handle_call/3,handle_cast/2]).
-export([handle_info/2,code_change/3,terminate/2]).

-record(player,{ name, round, opponents }).

create(Name) ->
    {ok,Pid} = gen_server:start_link(?MODULE,#player{ name = Name },[]),
    Pid.

name(Player) ->
    gen_server:call(Player,name).

round(Player, Round, Opponents) ->
    gen_server:call(Player,{round,Round,Opponents}).

serve(Player) ->
    gen_server:cast(Player,serve).

volley(Player, Volleys) ->
    gen_server:cast(Player,{volley,Volleys}).

init(Player) ->
    {ok,Player}.

handle_call(name,_From,Player) ->
    {reply,Player#player.name,Player};

handle_call({round,Round,Opponents},_From,Player) ->
    New = Player#player{round = Round, opponents = Opponents},
    {reply,none,New}.

handle_cast(serve,Player) ->
    [Opponent|_] = Player#player.opponents,
    Name = player:name( Opponent ),
    io:format( "~s serves to ~s~n", [ Player#player.name,
                                      Name ] ),
    player:volley( Opponent, 1 ),
    {noreply, Player};

handle_cast({volley,Volleys},Player) ->
    Okay = made_serve(),
    if
        Okay ->
            Opponent = choose(Player),
            io:format( "~s hits to ~s~n", [ Player#player.name,
                                            player:name( Opponent ) ] ),
            player:volley(Opponent, Volleys+1);
        true ->
            round:failed( Player#player.round, self(), Volleys )
    end,
    {noreply,Player};

handle_cast(_,_) -> fail.

made_serve() ->
    random:uniform() < 0.9.

choose(Player) ->
    lists:nth( random:uniform(3),Player#player.opponents ).

handle_info(_,_) -> fail.
code_change(_,_,_) -> fail.
terminate(_,_) -> fail.
