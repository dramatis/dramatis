-module(client).

-export([new/4]).
-export([name/1]).

-record(state,
        { name, increment, top, auction, current, max, winner }).

new( Name, Increment, Top, Auction ) ->
    spawn( fun() -> init( Name, Increment, Top, Auction ) end ).

name(Client) ->
    Client ! { name, self() },
    receive
        Result -> Result
    end.

init( Name, Increment, Top, Auction ) ->
    {Max,_} = auction:inquire( Auction ),
    loop( #state{name = Name,
                 increment = Increment,
                 top = Top,
                 auction = Auction,
                 current = 0,
                 max = Max} ).

loop(State)->

    if
        State#state.max >= State#state.top ->
            log(State,"too high for me"),
            NewState = State;
        
        State#state.current =< State#state.max ->
            Current = State#state.max + State#state.increment,

            sleep(),

            auction:offer( State#state.auction, Current ),

            NewState = State#state{current = Current};

        true ->
            NewState = State
    end,
            
    receive
        best_offer ->
            log( NewState,
                 io_lib:format("best_offer: ~p", [NewState#state.current]) ),
            NewerState = NewState;
        auction_over ->
            log( NewState, "auction_over" ),
            exit(ok),
            NewerState = NewState;
        { auction_concluded, _Seller, _Client } ->
            log( NewState, "auction_concluded" ),
            exit(ok),
            NewerState = NewState;
        { beaten_offer, MaxBid } ->
            log( NewState,
                 io_lib:format("beaten_offer: ~p", [MaxBid]) ),
            NewerState = NewState#state{ max = MaxBid };
        Any ->
            NewerState = NewState,
            exit(not_understood,Any)
    end,
    loop(NewerState).

log(State,String) ->
    io:format("client ~s: ~s~n", [State#state.name, String]).

sleep() -> receive after 1 + random:uniform(1000) -> true end.
