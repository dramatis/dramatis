-module(round).
-export([create/1,play/1,loser/1,volleys/1,players/1,failed/3]).

-record(round,{players,loser,volleys}).

create(Players) ->
    #round{players = Players}.

play(Round) ->
    Players = Round#round.players,
    {_,_,_,Server} = Players,
    player:serve( Server, Round ).

failed(Round,Player,Volleys) ->
    Round#round{loser = Player, volleys = Volleys}.

loser(Round) ->
    Round#round.loser.

volleys(Round) ->
    Round#round.volleys.

players(Round) ->
    Round#round.players.
