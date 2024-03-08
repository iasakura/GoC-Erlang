-module(channel).
-export([create/1, send/2, recv/1, peek/1]).


%% @doc create/1 creates a new channel with a given depth.
create(Depth) ->
    spawn(fun() -> loop([], Depth) end).


%% @doc send/2 sends a message to a channel.
send(ChannelPid, Message) ->
    ChannelPid ! {send, Message, self()}.


%% @doc recv/1 receives a message from a channel.
recv(ChannelPid) ->
    ChannelPid ! {recv, self()},
    receive
        {channel_reply, Reply} ->
            Reply
    end.


%% @doc peek/1 peeks a message from a channel.
peek(ChannelPid) ->
    ChannelPid ! {peek, self()},
    receive
        {channel_reply, Reply} ->
            Reply
    end.


loop(Queue, Depth) ->
    receive
        {send, Message, SenderPid} ->
            NewQueue = if
                           length(Queue) < Depth -> Queue ++ [{Message, SenderPid}];
                           true -> Queue
                       end,
            loop(NewQueue, Depth);
        {recv, ReceiverPid} ->
            case Queue of
                [] ->
                    ReceiverPid ! {channel_reply, empty},
                    loop(Queue, Depth);
                [{Message, _SenderPid} | Tail] ->
                    ReceiverPid ! {channel_reply, {ok, Message}},
                    loop(Tail, Depth)
            end;
        {peek, ReceiverPid} ->
            case Queue of
                [] ->
                    ReceiverPid ! {channel_reply, empty},
                    loop(Queue, Depth);
                [{Message, _SenderPid} | _] ->
                    ReceiverPid ! {channel_reply, {ok, Message}},
                    loop(Queue, Depth)
            end
    end.
