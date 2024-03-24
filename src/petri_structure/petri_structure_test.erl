-module(petri_structure_test).

-include_lib("eunit/include/eunit.hrl").


compose_test() ->
    PS = petri_structure:compose(mocks:pstruct1(), mocks:pstruct2()),
    ?debugFmt("~p~n", [PS]).
