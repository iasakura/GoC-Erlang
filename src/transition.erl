-module(transition).

-include("petri_structure.hrl").

-export([create/2]).


-spec(create(transition(), #{string() => pid()}) -> 'ok').
create(#transition{porality = Porality, address = Address, pre = Pre, post = Post, delta = Delta}, LocationTable) ->
    PreSorted = lists:sort(Pre),
    PostSorted = lists:sort(Post),
    PreLocations = maps:from_list(list:map(fun({location, Id}) -> {Id, maps:get(Id, LocationTable)} end, PreSorted)),
    PostLocations = maps:from_list(list:map(fun({location, Id}) -> {Id, maps:get(Id, LocationTable)} end, PostSorted)),
    spawn(fun() -> loop(Porality, Address, PreLocations, PostLocations, Delta) end).


%% すべてのロックを取得する。1つでも取得できなかった場合は、取得したロックを解放してnilを返す。
-spec(try_lock(read | write, #{string() => pid()}) -> ok | nil).
try_lock(LockType, Locks) ->
    %% デッドロック回避のため、Id順でロックを取得する。
    try_lock(LockType, lists:sort(maps:to_list(Locks)), []).


try_lock(_, [], _) -> ok;
try_lock(type, [{_, Loc} | Rest], Locked) ->
    res = case type of
              read -> locked_cell:read_lock(Loc);
              write -> locked_cell:write_lock(Loc)
          end,
    case res of
        ok -> try_lock(type, Rest, [Loc | Locked]);
        _ -> lists:foreach(fun(L) -> locked_cell:unlock(L) end, Locked), nil
    end.


-spec(fetch_pre_values(#{string() => pid()}) -> 'ok' | 'nil').
fetch_pre_values(PreLocations) ->
    maybe
        ok ?= try_lock(read, PreLocations),
        maps:map(fun(Id, L) -> {Id, locked_cell:read(L)} end, PreLocations)
    else
        _ -> nil
    end.


%% PreLocationsの値がPreTokilの値と一致しているかどうかをチェックする。
-spec(check_pre_tokil(tokil(), #{string() => pid()}) -> boolean()).
check_pre_tokil(PreTokil, PreLocations) ->
    maps:all(fun({Id, Value}) -> Value == locked_cell:read(maps:get(Id, PreLocations)) end, PreTokil).


-spec(update_with_post_values(address() | 'nil', #{string() => pid()}, #{string() => pid()}, tokil(), tokil()) -> 'ok' | 'nil').
update_with_post_values(Address, PreLocations, PostLocations, PreTokil, PostTokil) ->
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
-spec(loop(porality(), address() | 'nil', #{string() => pid()}, #{string() => pid()}, delta()) -> 'ok').
loop(Porality, Address, PreLocations, PostLocations, Delta) ->
    maybe
        PreTokil ?= fetch_pre_values(PreLocations),
        {ok, PostTokil} ?= Delta(PreTokil),
        ok ?= update_with_post_values(Address, PreLocations, PostLocations, PreTokil, PostTokil)
    end,
    loop(Porality, Address, PreLocations, PostLocations, Delta).
