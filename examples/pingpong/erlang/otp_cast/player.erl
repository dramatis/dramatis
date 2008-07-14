-module(player).
-behaviour(gen_server).
-export([create/1,volley/3]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3]).
-export([terminate/2]).

create( Name ) ->
    {ok,Pid} = gen_server:start_link(?MODULE,Name,[]),
    Pid.

volley(Player, Volleys, Opponent) ->
    gen_server:cast(Player, {volley, Volleys, Opponent}).

init(Name) ->
    {ok,{Name}}.

handle_cast({volley,Volleys,Opponent},State) ->
    { Name } = State,
    if
        Volleys == 0 ->
            io:format("~s: done~n",[Name]),
            {noreply,State};
        true ->
            if
                ( Volleys rem 500 == 0 ) or ( Volleys rem 500 == 1 ) ->
                    io:format("~s: volley ~p~n", [Name, Volleys]);
                true -> true
            end,
            volley(Opponent,Volleys-1,self()),
            {noreply,State}
    end.

handle_call(_,_,_) -> fail.
handle_info(_,_) -> fail.
code_change(_,_,_) -> fail.
terminate(_,_) -> fail.
