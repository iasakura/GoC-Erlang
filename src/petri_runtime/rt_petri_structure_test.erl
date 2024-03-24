-module(rt_petri_structure_test).

-include_lib("eunit/include/eunit.hrl").
-include("../petri_structure/petri_structure.hrl").


pstruct1_test() ->
    error_logger:tty(false),
    PS = rt_petri_structure:create(mocks:pstruct1()),
    ok = rt_petri_structure:write(PS, q, #token{domain = q}),
    Loop = fun Loop() ->
                   case rt_petri_structure:read(PS, a) of
                       {ok, #token{domain = V}} -> V;
                       _ -> erlang:yield(), Loop()
                   end
           end,
    ok = Loop().


pstruct2_test() ->
    PS = rt_petri_structure:create(mocks:pstruct2()),
    ok = rt_petri_structure:write(PS, q, #token{domain = q}),
    Loop = fun Loop() ->
                   case rt_petri_structure:read(PS, a) of
                       {ok, #token{domain = V}} -> V;
                       _ -> erlang:yield(), Loop()
                   end
           end,
    Res = Loop(),
    io:format("Res = ~p~n", [Res]),
    true = is_boolean(Res).
