-module(transition_test).

-include("petri_structure.hrl").
-include_lib("eunit/include/eunit.hrl").


transition1() ->
    #transition{
      porality = pos,
      address = nil,
      pre = [{location, "1"}],
      post = [],
      delta = fun(Tokil) ->
                      case Tokil of
                          #{"1" := #token{domain = q}} = V when map_size(V) =:= 1 -> {ok, #token{domain = 'ok'}};
                          _ -> nil
                      end
              end
     }.


transition1_test() ->
    Loc1 = locked_cell:create(),
    LocationTable = #{"1" => Loc1},
    Tr1 = transition:create(transition1(), LocationTable),
    ok = locked_cell:blocking_write_lock(Loc1),
    locked_cell:set(Loc1, #token{domain = q}),
    locked_cell:unlock(Loc1),
    erlang:yield(),
    erlang:yield(),
    erlang:yield(),
    erlang:yield(),
    erlang:yield(),
    erlang:yield(),
    erlang:yield(),
    {ok, #token{domain = 'ok'}} = transition:read(Tr1).
