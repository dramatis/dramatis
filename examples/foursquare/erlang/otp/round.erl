-module(round).
-behaviour(gen_server).
-export([create/1,play/1,loser/1,volleys/1,players/1,failed/3]).
-export([init/1,handle_call/3,handle_cast/2]).
-export([handle_info/2,code_change/3,terminate/2]).

-record(round,{players,loser,volleys,results = []}).

create(Players) ->
    {ok,Pid} = gen_server:start_link(?MODULE,Players,[]),
    Pid.

play(Round) ->
    gen_server:call(Round,play).

failed(Round,Player,Volleys) ->
    gen_server:call(Round,{failed,Player,Volleys}).

loser(Round) ->
    { Loser, _ } = gen_server:call(Round,results),
    Loser.

volleys(Round) ->
    { _, Volleys } = gen_server:call(Round,results),
    Volleys.

players(Round) ->
    gen_server:call(Round,players).

init(Players) ->
    Round = #round{players = Players},
    lists:foreach( fun(Player) ->
                           Opponents = [ Opponent || Opponent <- Players,
                                                     Opponent /= Player ],
                           player:round( Player, self(), Opponents )
                   end,
                   Players ),
    {ok,Round}.

handle_call(play,_From,Round) ->
    Players = Round#round.players,
    Server = lists:last(Players),
    player:serve( Server ),
    {reply,none,Round};

handle_call({failed,Loser,Volleys},_From,Round) ->
    Results = {Loser,Volleys},
    if 
        Round#round.results /= [] ->
            lists:foreach( fun(From) -> gen_server:reply(From,Results) end,
                           Round#round.results );
        true -> true
    end,
    {reply,
     none,
     Round#round{ loser = Loser, volleys = Volleys, results = [] }};

handle_call(results,From,Round) ->
    if
        Round#round.loser == undefined ->
            { noreply, Round#round{results = [From|Round#round.results]} };
        true ->
            { reply, {Round#round.loser,Round#round.volleys}, Round }
    end;

handle_call(players,_From,Round) ->
    {reply,Round#round.players,Round};

handle_call(_,_From,_Round) -> fail.

handle_cast(_,_) -> fail.
handle_info(_,_) -> fail.
code_change(_,_,_) -> fail.
terminate(_,_) -> fail.
