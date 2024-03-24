-module(rt_transition).

-define(NODEBUG, true).
-include("../petri_structure/petri_structure.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([create/2, read/1, write/2]).

-type(io() :: internal | {neg, pid()} | {pos, pid()}).


-spec(porality(address() | nil) -> pos | neg | internal).
porality(nil) -> internal;
porality(A) -> address:porality(A).


-spec(create(transition(), #{string() => pid()}) -> pid()).
create(#transition{address = Address, pre = Pre, post = Post, delta = Delta}, LocationTable) ->
    PreSorted = lists:sort(Pre),
    PostSorted = lists:sort(Post),
    PreLocations = maps:from_list(lists:map(fun({location, Id}) -> {Id, maps:get(Id, LocationTable)} end, PreSorted)),
    PostLocations = maps:from_list(lists:map(fun({location, Id}) -> {Id, maps:get(Id, LocationTable)} end, PostSorted)),
    IO = case porality(Address) of
             internal -> internal;
             neg -> {neg, rt_locked_cell:create()};
             pos -> {pos, rt_locked_cell:create()}
         end,
    spawn(fun() -> update_loop(IO, PreLocations, PostLocations, Delta) end),
    spawn(fun() -> receive_loop(IO, PreLocations, PostLocations, Delta) end).


%% すべてのロックを取得する。1つでも取得できなかった場合は、取得したロックを解放してnilを返す。
-spec(try_lock(read | write, #{string() => pid()}) -> ok | nil).
try_lock(LockType, Locks) ->
    %% デッドロック回避のため、Id順でロックを取得する。
    try_lock(LockType, lists:sort(maps:to_list(Locks)), []).


-spec(try_lock(read | write, [{string(), pid()}], [pid()]) -> ok | nil).
try_lock(_, [], _) -> ok;
try_lock(LockType, [{_, Loc} | Rest], Locked) ->
    Res = case LockType of
              read -> rt_locked_cell:read_lock(Loc);
              write -> rt_locked_cell:write_lock(Loc)
          end,
    case Res of
        ok -> try_lock(LockType, Rest, [Loc | Locked]);
        _ -> lists:foreach(fun(L) -> rt_locked_cell:unlock(L) end, Locked), nil
    end.


-spec(fetch_pre_values(#{string() => pid()}) -> {'ok', tokil()} | 'nil').
fetch_pre_values(PreLocations) -> {ok, maps:map(fun(_, L) -> rt_locked_cell:unsafe_read(L) end, PreLocations)}.


%% PreLocationsの値がPreTokilの値と一致しているかどうかをチェックする。
-spec(check_pre_tokil(tokil(), #{string() => pid()}) -> boolean()).
check_pre_tokil(PreTokil, PreLocations) ->
    maps:fold(fun(Id, Value, Acc) -> Acc andalso Value == rt_locked_cell:read(maps:get(Id, PreLocations)) end, true, PreTokil).


-spec(update_with_post_values(#{string() => pid()}, #{string() => pid()}, tokil(), tokil()) -> 'ok' | 'nil').
update_with_post_values(PreLocations, PostLocations, PreTokil, PostTokil) ->
    Locs = maps:merge(PreLocations, PostLocations),
    L = try_lock(write, Locs),
    ?debugFmt("Locs ~p, Lock: ~p~n", [Locs, L]),
    case L of
        nil -> nil;
        ok ->
            Res =
                maybe
                    true ?= check_pre_tokil(PreTokil, PreLocations),
                    maps:foreach(fun(_, Loc) ->
                                         rt_locked_cell:set(Loc, nil)
                                 end,
                                 PreLocations),
                    maps:foreach(fun(Id, Loc) ->
                                         ?debugFmt("Id: ~p, Loc: ~p, Val: ~p~n", [Id, Loc, maps:get(Id, PostTokil)]),
                                         rt_locked_cell:set(Loc, maps:get(Id, PostTokil))
                                 end,
                                 PostLocations)
                else
                    _ -> nil
                end,
            lists:foreach(fun({_, Cell}) -> rt_locked_cell:unlock(Cell) end, lists:sort(maps:to_list(Locs))),
            Res
    end.


%% PreLocations, PostLocationsはId順でソートされている必要がある （デッドロック回避のため）。
-spec(update_loop(io(),
                  #{string() => pid()},
                  #{string() => pid()},
                  delta()) -> 'ok').
update_loop(IO, PreLocations, PostLocations, Delta) ->
    ?debugFmt("io: ~p, preloc: ~p~n", [IO, PreLocations]),
    case IO of
        internal ->
            maybe
                {ok, PreTokil} ?= fetch_pre_values(PreLocations),
                ?debugFmt("pretokil: ~p~n", [PreTokil]),
                {ok, PostTokil} ?= Delta(PreTokil),
                ?debugFmt("posttokil: ~p~n", [PostTokil]),
                ok ?= update_with_post_values(PreLocations, PostLocations, PreTokil, PostTokil)
            end,
            update_loop(IO, PreLocations, PostLocations, Delta);
        {neg, Cell} ->
            maybe
                % 入力から更新
                Val = rt_locked_cell:unsafe_read(Cell),
                ?debugFmt("pretoken: ~p~n", [Val]),
                %
                {ok, PostTokil} ?= Delta(Val),
                ?debugFmt("posttokil: ~p~n", [PostTokil]),
                ok ?= update_with_post_values(PreLocations, PostLocations, #{}, PostTokil),
                rt_locked_cell:with_write_lock(Cell, fun() -> rt_locked_cell:set(Cell, nil) end)
            end,
            update_loop(IO, PreLocations, PostLocations, Delta);
        {pos, Cell} ->
            maybe
                {ok, PreTokil} ?= fetch_pre_values(PreLocations),
                ?debugFmt("pretokil: ~p~n", [PreTokil]),
                {ok, #token{domain = _} = Token} ?= Delta(PreTokil),
                ?debugFmt("token: ~p~n", [Token]),
                % 出力を更新
                ok ?= update_with_post_values(PreLocations, PostLocations, PreTokil, #{}),
                rt_locked_cell:with_write_lock(Cell, fun() -> rt_locked_cell:set(Cell, Token) end),
                ?debugFmt("done~n", [])
            end,
            update_loop(IO, PreLocations, PostLocations, Delta)
    end.


%% PreLocations, PostLocationsはId順でソートされている必要がある （デッドロック回避のため）。
-spec(receive_loop(io(), #{string() => pid()}, #{string() => pid()}, delta()) -> 'ok').
receive_loop(IO, PreLocations, PostLocations, Delta) ->
    receive
        {From, read} ->
            case IO of
                {pos, Cell} ->
                    begin
                        case rt_locked_cell:unsafe_read(Cell) of
                            #token{domain = _} = Val ->
                                ok = rt_locked_cell:blocking_read_lock(Cell),
                                Val = rt_locked_cell:read(Cell),
                                ?debugFmt("Val: ~p~n", [Val]),
                                rt_locked_cell:unlock(Cell),
                                From ! {self(), {ok, Val}};
                            _ -> From ! {self(), nil}
                        end
                    end;
                _ -> From ! {self(), nil}
            end;
        {From, write, V} ->
            case IO of
                {neg, Cell} ->
                    begin
                        ok = rt_locked_cell:blocking_write_lock(Cell),
                        rt_locked_cell:set(Cell, V),
                        rt_locked_cell:unlock(Cell),
                        From ! {self(), ok}
                    end;
                _ -> From ! {self(), nil}
            end
    end,
    receive_loop(IO, PreLocations, PostLocations, Delta).


-spec(read(pid()) -> {ok, any()} | nil).
read(Pid) -> Pid ! {self(), read}, receive {Pid, Value} -> Value end.


-spec(write(pid(), any()) -> ok | nil).
write(Pid, V) -> Pid ! {self(), write, V}, receive {Pid, Res} -> Res end.
