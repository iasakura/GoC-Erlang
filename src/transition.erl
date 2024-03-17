-module(transition).

-include("petri_structure.hrl").

-export([create/2, read/1, write/2]).

-type(io() :: internal | {neg, pid()} | {pos, pid()}).


-spec(create(transition(), #{string() => pid()}) -> pid()).
create(#transition{porality = Porality, pre = Pre, post = Post, delta = Delta}, LocationTable) ->
    PreSorted = lists:sort(Pre),
    PostSorted = lists:sort(Post),
    PreLocations = maps:from_list(lists:map(fun({location, Id}) -> {Id, maps:get(Id, LocationTable)} end, PreSorted)),
    PostLocations = maps:from_list(lists:map(fun({location, Id}) -> {Id, maps:get(Id, LocationTable)} end, PostSorted)),
    IO = case Porality of
             internal -> internal;
             neg -> {neg, locked_cell:create()};
             pos -> {pos, locked_cell:create()}
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
              read -> locked_cell:read_lock(Loc);
              write -> locked_cell:write_lock(Loc)
          end,
    case Res of
        ok -> try_lock(LockType, Rest, [Loc | Locked]);
        _ -> lists:foreach(fun(L) -> locked_cell:unlock(L) end, Locked), nil
    end.


-spec(fetch_pre_values(#{string() => pid()}) -> {'ok', tokil()} | 'nil').
fetch_pre_values(PreLocations) ->
    maybe
        ok ?= try_lock(read, PreLocations),
        {ok, maps:map(fun(_, L) -> locked_cell:read(L) end, PreLocations)}
    else
        _ -> nil
    end.


%% PreLocationsの値がPreTokilの値と一致しているかどうかをチェックする。
-spec(check_pre_tokil(tokil(), #{string() => pid()}) -> boolean()).
check_pre_tokil(PreTokil, PreLocations) ->
    maps:fold(fun(Id, Value, Acc) -> Acc andalso Value == locked_cell:read(maps:get(Id, PreLocations)) end, true, PreTokil).


-spec(update_with_post_values(#{string() => pid()}, #{string() => pid()}, tokil(), tokil()) -> 'ok' | 'nil').
update_with_post_values(PreLocations, PostLocations, PreTokil, PostTokil) ->
    Locs = maps:merge(PreLocations, PostLocations),
    case try_lock(write, Locs) of
        nil -> nil;
        ok ->
            maybe
                true ?= check_pre_tokil(PreTokil, PreLocations),
                maps:foreach(fun(_, Loc) -> locked_cell:set(Loc, nil) end, PreLocations),
                maps:foreach(fun(Id, Loc) -> locked_cell:set(Loc, maps:get(PostTokil, Id)) end, PostLocations)
            else
                _ -> nil
            end
    end.


%% PreLocations, PostLocationsはId順でソートされている必要がある （デッドロック回避のため）。
-spec(update_loop(io(),
                  #{string() => pid()},
                  #{string() => pid()},
                  delta()) -> 'ok').
update_loop(IO, PreLocations, PostLocations, Delta) ->
    io:format("io: ~p, preloc: ~p~n", [IO, PreLocations]),
    case IO of
        internal ->
            maybe
                {ok, PreTokil} ?= fetch_pre_values(PreLocations),
                {ok, PostTokil} ?= Delta(PreTokil),
                ok ?= update_with_post_values(PreLocations, PostLocations, PreTokil, PostTokil)
            end,
            update_loop(IO, PreLocations, PostLocations, Delta);
        {neg, Cell} ->
            maybe
                % 入力から更新
                ok = locked_cell:blocking_read_lock(Cell),
                Val = locked_cell:read(Cell),
                locked_cell:unlock(Cell),
                %
                {ok, PostTokil} ?= Delta(Val),
                ok ?= update_with_post_values(PreLocations, PostLocations, #{}, PostTokil)
            end,
            update_loop(IO, PreLocations, PostLocations, Delta);
        {pos, Cell} ->
            maybe
                {ok, PreTokil} ?= fetch_pre_values(PreLocations),
                io:format("pretokil: ~p~n", [PreTokil]),
                {ok, #token{domain = _} = Token} ?= Delta(PreTokil),
                io:format("token: ~p", [Token]),
                % 出力を更新
                ok = locked_cell:blocking_write_lock(Cell),
                locked_cell:set(Cell, Token),
                locked_cell:unlock(Cell)
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
                        ok = locked_cell:blocking_read_lock(Cell),
                        Val = locked_cell:read(Cell),
                        locked_cell:unlock(Cell),
                        From ! {self(), {ok, Val}}
                    end;
                _ -> From ! {self(), nil}
            end;
        {From, write, V} ->
            case IO of
                {neg, Cell} ->
                    begin
                        ok = locked_cell:blocking_write_lock(Cell),
                        locked_cell:set(Cell, V),
                        locked_cell:unlock(Cell),
                        From ! ok
                    end;
                _ -> From ! {self(), nil}
            end
    end,
    receive_loop(IO, PreLocations, PostLocations, Delta).


read(Pid) -> Pid ! {self(), read}, receive {Pid, Value} -> Value end.


write(Pid, V) -> Pid ! {self(), write, V}, receive {Pid, Res} -> Res end.
