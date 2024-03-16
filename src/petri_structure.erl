%% Geometry of Causality compiler to Erlang processes.
-module(petri_structure).

-export([create/1]).

-include("petri_structure.hrl").


-spec create(pstruct()) -> 'ok'.
create(#pstruct{locations = Locations, transitions = Transitions}) ->
    LocationTable = maps:from_list(lists:map(fun(#location{id = Id}) -> {Id, locked_cell:create()} end, Locations)),
    lists:foreach(fun(T) -> transition:create(T, LocationTable) end, Transitions),
    ok.
