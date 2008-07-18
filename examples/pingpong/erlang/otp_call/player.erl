% Note: this doesn't work because of the way gen_server does selective receive

-module(player).
-behaviour(gen_server).
-export([create/1,volley/3]).
-export([init/1,handle_call/3,handle_cast/2]).
-export([handle_info/2,code_change/3,terminate/2]).

create( Name ) ->
    {ok,Pid} = gen_server:start_link(?MODULE,Name,[]),
    Pid.

volley(Player, Volleys, Opponent) ->
    gen_server:call(Player, {volley, Volleys, Opponent}).

init(Name) ->
    {ok,{Name}}.


handle_call({volley,Volleys,Opponent},From,State) ->
    { Name } = State,
    if
        Volleys == 0 ->
            io:format("~s: done~n",[Name]),
            {reply,[],State};
        true ->
            if
                ( Volleys rem 500 == 0 ) or ( Volleys rem 500 == 1 ) ->
                    io:format("~s: volley ~p~n", [Name, Volleys]);
                true -> true
            end,
            volley(Opponent,Volleys-1,self()),
            {reply,[],State}
    end.

handle_cast(_,_) -> fail.
handle_info(_,_) -> fail.
code_change(_,_,_) -> fail.
terminate(_,_) -> fail.
