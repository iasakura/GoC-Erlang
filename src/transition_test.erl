-module(transition_test).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").
-include("petri_structure.hrl").


transition1() ->
    #transition{
      address = a,
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
    ?debugFmt("~p~n", [ok]),
    locked_cell:unlock(Loc1),
    Loop = fun Loop() ->
                   case transition:read(Tr1) of
                       {ok, #token{domain = 'ok'}} -> 'ok';
                       X -> ?debugFmt("~p~n", [X]), erlang:yield(), Loop()
                   end
           end,
    ok = Loop().


-spec(transition2() -> transition()).
transition2() ->
    #transition{
      address = q,
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

    Loop = fun Loop() ->
                   V = maybe
                           ok ?= locked_cell:read_lock(Loc1),
                           X = locked_cell:read(Loc1),
                           locked_cell:unlock(Loc1),
                           X
                       else
                           _ -> nil
                       end,
                   case V of
                       #token{domain = 'ok'} -> 'ok';
                       V0 -> ?debugFmt("X = ~p~n", [V0]), erlang:yield(), Loop()
                   end
           end,
    ok = Loop().
