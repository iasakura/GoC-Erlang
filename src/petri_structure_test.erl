-module(petri_structure_test).

-include_lib("eunit/include/eunit.hrl").
-include("petri_structure.hrl").

-define(NODEBUG, true).


pstruct1() ->
    #pstruct{
      locations = [#location{id = "1"}, #location{id = "2"}, #location{id = "3"}, #location{id = "4"}, #location{id = "5"}, #location{id = "6"}],
      transitions =
          [#transition{
             address = q,
             pre = [],
             post = [#location{id = "1"}, #location{id = "2"}],
             delta = fun(Token) -> case Token of #token{domain = q} -> {ok, #{"1" => #token{domain = q}, "2" => #token{domain = q}}}; _ -> nil end end
            },
           #transition{
             address = nil,
             pre = [#location{id = "1"}],
             post = [#location{id = "3"}],
             delta = fun(Token) -> case Token of #{"1" := #token{domain = q}} -> {ok, #{"3" => #token{domain = 'ok'}}}; _ -> nil end end
            },
           #transition{
             address = nil,
             pre = [#location{id = "3"}],
             post = [#location{id = "5"}],
             delta = fun(Token) -> case Token of #{"3" := #token{domain = V}} -> {ok, #{"5" => #token{domain = V}}}; _ -> nil end end
            },
           #transition{
             address = nil,
             pre = [#location{id = "2"}],
             post = [#location{id = "4"}],
             delta = fun(Token) -> case Token of #{"2" := #token{domain = q}} -> {ok, #{"4" => #token{domain = ok}}}; _ -> nil end end
            },
           #transition{
             address = nil,
             pre = [#location{id = "4"}],
             post = [#location{id = "6"}],
             delta = fun(Token) -> case Token of #{"4" := #token{domain = V}} -> {ok, #{"6" => #token{domain = V}}}; _ -> nil end end
            },
           #transition{
             address = a,
             pre = [#location{id = "5"}, #location{id = "6"}],
             post = [],
             delta = fun(Token) -> case Token of #{"5" := #token{domain = ok}, "6" := #token{domain = V}} -> {ok, #token{domain = V}}; _ -> nil end end
            }]
     }.


pstruct1_test() ->
    error_logger:tty(false),
    PS = petri_structure:create(pstruct1()),
    ok = petri_structure:write(PS, q, #token{domain = q}),
    Loop = fun Loop() ->
                   case petri_structure:read(PS, a) of
                       {ok, #token{domain = V}} -> V;
                       _ -> erlang:yield(), Loop()
                   end
           end,
    Loop().
