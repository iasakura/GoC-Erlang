%% Geometry of Causality compiler to Erlang processes.
-module(goc_erlang).

-export([compile/1]).

-include("petri_structure.hrl").


-spec compile(pstruct()) -> 'ok'.
compile(#pstruct{delta = Delta, locations = Locations, transitions = Transitions}) ->
    LocationTable = maps:from_list(lists:map(fun(#location{id = Id}) -> {Id, locked_cell:create()} end, Locations)),
    lists:foreach(fun(T) -> compile_transition(T, LocationTable, Delta) end, Transitions),
    ok.
