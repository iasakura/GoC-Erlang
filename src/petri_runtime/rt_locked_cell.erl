-module(rt_locked_cell).

-export([create/0,
         read_lock/1,
         blocking_read_lock/1,
         write_lock/1,
         blocking_write_lock/1,
         unlock/1,
         read/1,
         unsafe_read/1,
         set/2,
         with_read_lock/2,
         with_write_lock/2]).


-spec create() -> pid().
create() ->
    spawn(fun() -> loop({reads, []}, nil) end).


-spec read_lock(pid()) -> 'ok' | nil.
read_lock(Pid) -> Pid ! {self(), read_lock}, receive {Pid, ok} -> ok; {Pid, nil} -> nil end.


blocking_read_lock(L) ->
    case read_lock(L) of
        ok -> ok;
        _ -> blocking_write_lock(L)
    end.


-spec write_lock(pid()) -> 'ok' | nil.
write_lock(Pid) -> Pid ! {self(), write_lock}, receive {Pid, ok} -> ok; {Pid, nil} -> nil end.


blocking_write_lock(L) ->
    case write_lock(L) of
        ok -> ok;
        _ -> blocking_write_lock(L)
    end.


-spec unlock(pid()) -> 'ok' | {error, string()}.
unlock(Pid) -> Pid ! {self(), unlock}, receive {Pid, ok} -> ok; {Pid, {error, Reason}} -> {error, Reason} end.


-spec read(pid()) -> any() | {error, string()}.
read(Pid) -> Pid ! {self(), read}, receive {Pid, Value} -> Value; {Pid, {error, Reason}} -> {error, Reason} end.


-spec unsafe_read(pid()) -> any() | {error, string()}.
unsafe_read(Pid) -> Pid ! {self(), unsafe_read}, receive {Pid, Value} -> Value; {Pid, {error, Reason}} -> {error, Reason} end.


-spec set(pid(), any()) -> 'ok' | {error, string()}.
set(Pid, NewValue) ->
    io:format("pid: ~p, arg: ~p~n", [Pid, NewValue]),
    Pid ! {self(), set, NewValue},
    receive {Pid, Value} -> Value; {Pid, {error, Reason}} -> {error, Reason} end.


-spec loop({'reads', [pid()]} | {'writes', pid()}, any()) -> no_return().
loop(Locks, Value) ->
    receive
        {From, read_lock} ->
            case Locks of
                {reads, Reads} -> From ! {self(), ok}, loop({reads, [From | Reads]}, Value);
                {writes, _} -> From ! {self(), nil}, loop(Locks, Value)
            end;
        {From, write_lock} ->
            case Locks of
                {reads, []} -> From ! {self(), ok}, loop({writes, From}, Value);
                {reads, _} -> From ! {self(), nil}, loop(Locks, Value);
                {writes, _} -> From ! {self(), nil}, loop(Locks, Value)
            end;
        {From, unlock} ->
            case Locks of
                {reads, Reads} ->
                    {Ps, Rest} = lists:partition(fun(P) -> P == From end, Reads),
                    case Ps of
                        [] -> From ! {self(), {error, "You haven't locked yet"}}, loop(Locks, Value);
                        _ -> From ! {self(), 'ok'}, loop({reads, Rest}, Value)
                    end;
                {writes, Write} ->
                    if
                        Write == From -> From ! {self(), 'ok'}, loop({reads, []}, Value);
                        true -> From ! {self(), {error, "You haven't locked yet"}}, loop(Locks, Value)
                    end
            end;
        {From, read} ->
            case Locks of
                {reads, Reads} ->
                    case lists:member(From, Reads) of
                        true -> From ! {self(), Value}, loop(Locks, Value);
                        false -> From ! {self(), {error, "You don't have lock"}}, loop(Locks, Value)
                    end;
                {writes, Write} ->
                    if
                        From =:= Write -> From ! {self(), Value}, loop(Locks, Value);
                        true -> From ! {self(), {error, "You don't have lock"}}, loop(Locks, Value)
                    end
            end;
        {From, unsafe_read} -> From ! {self(), Value}, loop(Locks, Value);
        {From, set, NewValue} ->
            case Locks of
                {reads, _} -> From ! {self(), "You don't have lock"}, loop(Locks, Value);
                {writes, Write} ->
                    if
                        From =:= Write -> From ! {self(), 'ok'}, loop(Locks, NewValue);
                        true -> From ! {self(), {error, "You don't have lock"}}, loop(Locks, Value)
                    end
            end
    end.


-spec with_read_lock(pid(), fun(() -> any())) -> any() | {error, string()}.
with_read_lock(Pid, Fun) ->
    maybe
        ok ?= read_lock(Pid),
        R = Fun(),
        unlock(Pid),
        R
    else
        _ -> {error, "Failed to lock"}
    end.


-spec with_write_lock(pid(), fun(() -> any())) -> any() | {error, string()}.
with_write_lock(Pid, Fun) ->
    maybe
        ok ?= write_lock(Pid),
        R = Fun(),
        unlock(Pid),
        R
    else
        _ -> {error, "Failed to lock"}
    end.
