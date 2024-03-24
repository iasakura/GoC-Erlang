-module(petri_structure).

-include("petri_structure.hrl").

-export([compose/2]).


-spec(synchronizable(address(), address()) -> boolean()).
synchronizable(A1, A2) ->
    case {A1, A2} of
        {{turnstile, right, M}, {turnstile, left, M}} -> true;
        {_, _} -> false
    end.


-spec(inj_loc(location(), 1 | 2) -> location()).
inj_loc(#location{id = A}, Idx) -> #location{id = [Idx | A]}.


-spec(prj_loc(location(), 1 | 2) -> {ok, location()} | nil).
prj_loc(#location{id = [Idx1 | A]}, Idx2) when Idx1 =:= Idx2 -> {ok, #location{id = A}};
prj_loc(_, _) -> nil.


-spec(inj_tokil(tokil(), 1 | 2) -> tokil()).
inj_tokil(Tokil, Idx) ->
    #{ inj_loc(Loc, Idx) => Val || Loc := Val <- Tokil }.


-spec(prj_tokil(tokil(), 1 | 2) -> {ok, tokil()} | nil).
prj_tokil(Tokil, Idx) ->
    try
        {ok, maps:map(fun(Loc, Val) ->
                              case prj_loc(Loc, Idx) of
                                  {ok, NewLoc} -> {NewLoc, Val};
                                  nil -> throw(error)
                              end
                      end,
                      Tokil)}
    catch
        _ -> nil
    end.


-spec(map_inj_delta(delta(), 1 | 2) -> delta()).
map_inj_delta(Delta, Idx) ->
    fun(Tokil) ->
            PrjTokil = case Tokil of
                           #token{} -> Tokil;
                           _ -> prj_tokil(Tokil, Idx)
                       end,
            Res = Delta(PrjTokil),
            case Res of
                {ok, #token{} = NewToken} -> NewToken;
                {ok, NewTokil} -> inj_tokil(NewTokil, Idx);
                _ -> nil
            end
    end.


-spec(compose(pstruct(), pstruct()) -> pstruct()).
compose(#pstruct{locations = L1, transitions = T1}, #pstruct{locations = L2, transitions = T2}) ->
    #pstruct{
      locations = lists:map(fun(L) -> inj_loc(L, 1) end, L1) ++ lists:map(fun(L) -> inj_loc(L, 2) end, L2),
      transitions = compose_transition(T1, T2)
     }.


-spec(synchronize([transition()], [transition()]) -> {[transition()], [{transition(), transition()}], [transition()]}).
synchronize(TS1, TS2) ->
    NewTS1 = [ T || T <- TS1,
                    T#transition.address =:= nil orelse
                    case T#transition.address of
                        {turnstile, right, _} -> false;
                        _ -> true
                    end ],
    NewTS2 = [ T || T <- TS2,
                    T#transition.address =:= nil orelse
                    case T#transition.address of
                        {turnstile, left, _} -> false;
                        _ -> true
                    end ],
    TSync = [ {Tr1, Tr2} || Tr1 <- NewTS1,
                            Tr2 <- NewTS2,
                            synchronizable(Tr1#transition.address, Tr2#transition.address) ],
    {NewTS1, TSync, NewTS2}.


% T1: M -> N, T2: N -> P,
-spec compose_transition([transition()], [transition()]) -> [transition()].
compose_transition(T1, T2) ->
    {TS1, TSync, TS2} = synchronize(T1, T2),
    [ #transition{
        address = A,
        pre = [ inj_loc(Loc, 1) || Loc <- Pre ],
        post = [ inj_loc(Loc, 1) || Loc <- Post ],
        delta = map_inj_delta(Delta, 1)
       } || #transition{address = A, pre = Pre, post = Post, delta = Delta} <- TS1 ] ++
    [ #transition{
        address = A,
        pre = [ inj_loc(Loc, 2) || Loc <- Pre ],
        post = [ inj_loc(Loc, 2) || Loc <- Post ],
        delta = map_inj_delta(Delta, 2)
       } || #transition{address = A, pre = Pre, post = Post, delta = Delta} <- TS2 ] ++
    [ #transition{
        address = nil,
        pre = case {address:porality(A1), address:porality(A2)} of
                  {pos, neg} -> [ inj_loc(Loc, 1) || Loc <- Pre1 ];
                  {neg, pos} -> [ inj_loc(Loc, 2) || Loc <- Pre2 ]
              end,
        post = case {address:porality(A1), address:porality(A2)} of
                   {pos, neg} -> [ inj_loc(Loc, 2) || Loc <- Post2 ];
                   {neg, pos} -> [ inj_loc(Loc, 1) || Loc <- Post1 ]
               end,
        delta = case {address:porality(A1), address:porality(A2)} of
                    {pos, neg} ->
                        fun(Tokil) ->
                                maybe
                                    {ok, ProjTokil} = prj_tokil(Tokil, 1),
                                    {ok, #token{} = Token} ?= Delta1(ProjTokil),
                                    {ok, NewTokil} ?= Delta2(Token),
                                    inj_tokil(NewTokil, 2)
                                else
                                    _ -> nil
                                end
                        end;
                    {neg, pos} ->
                        fun(Tokil) ->
                                maybe
                                    {ok, ProjTokil} = prj_tokil(Tokil, 2),
                                    {ok, #token{} = Token} ?= Delta2(ProjTokil),
                                    {ok, NewTokil} ?= Delta1(Token),
                                    inj_tokil(NewTokil, 1)
                                else
                                    _ -> nil
                                end
                        end
                end
       } || {#transition{address = A1, pre = Pre1, post = Post1, delta = Delta1},
             #transition{address = A2, pre = Pre2, post = Post2, delta = Delta2}} <- TSync ].
