%% Geometry of Causality compiler to Erlang processes.
-module(petri_structure).

-export([create/1, read/2, write/3]).

-include("petri_structure.hrl").


-spec create(pstruct()) -> 'ok'.
create(#pstruct{locations = Locations, transitions = Transitions}) ->
    LocationTable = maps:from_list(lists:map(fun(#location{id = Id}) -> {Id, locked_cell:create()} end, Locations)),
    TransitionProcesses = lists:map(fun(T) -> {T, transition:create(T, LocationTable)} end, Transitions),
    Externals = lists:filter(fun({#transition{address = Address}, _}) -> Address =/= nil end, TransitionProcesses),
    ExternalMap = maps:from_list(lists:map(fun({T, TP}) -> {T#transition.address, TP} end, Externals)),
    spawn(fun() -> loop(ExternalMap) end).


-spec loop(#{address() => transition()}) -> no_return().
loop(ExternalMap) ->
    receive
        {From, write, Address, Value} ->
            maybe
                neg ?= address:porality(Address),
                T ?= maps:get(Address, ExternalMap, nil),
                true = T /= nil,
                transition:write(T, Value),
                From ! {self(), ok}
            else
                _ -> From ! {self(), {error, "invalid address"}}
            end,
            loop(ExternalMap);
        {From, read, Address} ->
            maybe
                pos ?= address:porality(Address),
                T ?= maps:get(Address, ExternalMap, nil),
                true = T /= nil,
                Value = transition:read(T),
                From ! {self(), {ok, Value}}
            else
                _ -> From ! {self(), {error, "invalid address"}}
            end,
            loop(ExternalMap)
    end.


read(P, Address) ->
    P ! {self(), read, Address},
    receive
        {P, {ok, Value}} -> Value;
        {P, {error, Reason}} -> {error, Reason}
    end.


write(P, Address, Value) ->
    P ! {self(), write, Address, Value},
    receive
        {P, ok} -> ok;
        {P, {error, Reason}} -> {error, Reason}
    end.
