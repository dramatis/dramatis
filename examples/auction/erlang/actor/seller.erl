-module(seller).
-export([new/0]).
-export([failed/1]).
-export([winner/2]).

new() ->
    spawn( fun() -> loop() end ).

failed(Seller) ->
    Seller ! { failed }.

winner(Seller,Client) ->
    Seller ! { winner, Client }.

loop() ->
    receive
        { failed } ->
            exit(ok);
        { winner, _Client } ->
            exit(ok)
    end,
    loop().
