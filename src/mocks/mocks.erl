-module(mocks).

-include("../petri_structure/petri_structure.hrl").

-export([pstruct1/0, pstruct2/0]).


-spec(pstruct1() -> pstruct()).
% Fig. 11
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


-spec(pstruct2() -> pstruct()).
% Fig. 12
pstruct2() ->
    #pstruct{
      locations = [#location{id = "1"}, #location{id = "2"}, #location{id = "3"}, #location{id = "4"}, #location{id = "5"}],
      transitions =
          [#transition{
             address = q,
             pre = [],
             post = [#location{id = "1"}, #location{id = "2"}, #location{id = "3"}],
             delta = fun(Token) ->
                             case Token of
                                 #token{domain = q} ->
                                     {ok,
                                      #{
                                        "1" => #token{domain = 1},
                                        "2" => #token{domain = 0},
                                        "3" => #token{domain = q}
                                       }};
                                 _ -> nil
                             end
                     end
            },
           #transition{
             address = nil,
             pre = [#location{id = "1"}, #location{id = "2"}],
             post = [#location{id = "2"}, #location{id = "4"}],
             delta = fun(Tokil) ->
                             case Tokil of
                                 #{
                                   "1" := #token{domain = N},
                                   "2" := #token{domain = _}
                                  } ->
                                     {ok,
                                      #{
                                        "4" => #token{domain = ok},
                                        "2" => #token{domain = N}
                                       }};
                                 _ -> nil
                             end
                     end
            },
           #transition{
             address = nil,
             pre = [#location{id = "2"}, #location{id = "3"}],
             post = [#location{id = "2"}, #location{id = "5"}],
             delta = fun(Tokil) ->
                             case Tokil of
                                 #{
                                   "2" := #token{domain = N},
                                   "3" := #token{domain = q}
                                  } ->
                                     {ok,
                                      #{
                                        "2" => #token{domain = N},
                                        "5" => #token{domain = N}
                                       }};
                                 _ -> nil
                             end
                     end
            },
           #transition{
             address = a,
             pre = [#location{id = "4"}, #location{id = "5"}],
             post = [],
             delta =
                 fun(Tokil) ->
                         case Tokil of #{"4" := #token{domain = ok}, "5" := #token{domain = N}} -> {ok, #token{domain = N =:= 0}}; _ -> nil end
                 end
            }]
     }.
