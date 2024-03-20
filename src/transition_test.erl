-module(transition_test).

-include_lib("eunit/include/eunit.hrl").
-include("petri_structure.hrl").


transition1() ->
    #transition{
      address = nil,
      pre = [#location{id = "1"}],
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


-spec(transition2() -> transition()).
transition2() ->
    #transition{
      address = nil,
      pre = [],
      post = [#location{id = "1"}],
      delta = fun(Token) ->
                      case Token of
                          #token{domain = q} -> {ok, #{"1" => #token{domain = ok}}};
                          _ -> nil
                      end
              end
     }.


transition2_test() ->
    Loc1 = locked_cell:create(),
    LocationTable = #{"1" => Loc1},
    Tr2 = transition:create(transition2(), LocationTable),
    transition:write(Tr2, #token{domain = q}),
    erlang:yield(),
    erlang:yield(),
    erlang:yield(),
    erlang:yield(),
    ok = locked_cell:blocking_read_lock(Loc1),
    #token{domain = 'ok'} = locked_cell:read(Loc1),
    locked_cell:unlock(Loc1).
