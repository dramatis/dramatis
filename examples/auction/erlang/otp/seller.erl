-module(seller).
-behaviour(gen_server).
-export([new/0]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3]).
-export([terminate/2]).

new() ->
    {ok,Pid} = gen_server:start_link(?MODULE,[],[]),
    Pid.

init(_) ->
    {ok,{}}.


handle_call(_,_,_) -> fail.
handle_cast(_,_) -> fail.
handle_info(_,_) -> fail.
code_change(_,_,_) -> fail.
terminate(_,_) -> fail.
