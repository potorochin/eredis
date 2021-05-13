%%
%% eredis_pubsub_client
%%
%% This client implements a subscriber to a Redis pubsub channel. It
%% is implemented in the same way as eredis_client, except channel
%% messages are streamed to the controlling process. Messages are
%% queued and delivered when the client acknowledges receipt.
%%
%% There is one consuming process per eredis_sub_client.
%% @private
-module(eredis_sub_client).
-behaviour(gen_server).
-include("eredis.hrl").
-include("eredis_sub.hrl").


-define(CONNECT_TIMEOUT, 5000).
-define(RECONNECT_SLEEP, 100).

%% API
-export([start_link/8, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%
%% API
%%

% -spec start_link(Host::list(),
%                  Port::integer(),
%                  Password::string(),
%                  ReconnectSleep::reconnect_sleep(),
%                  MaxQueueSize::integer() | infinity,
%                  QueueBehaviour::drop | exit, TlsOptions, Transport) ->
%                         {ok, Pid::pid()} | {error, Reason::term()}.
start_link(Host, Port, Password, ReconnectSleep, MaxQueueSize, QueueBehaviour, TlsOptions, Transport) ->
    Args = [Host, Port, Password, ReconnectSleep, MaxQueueSize, QueueBehaviour, TlsOptions, Transport],
    gen_server:start_link(?MODULE, Args, []).


stop(Pid) ->
    gen_server:call(Pid, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Host, Port, Password, _, MaxQueueSize, QueueBehaviour, TlsOptions, Transport]) ->
    % Transport = tls,
    
    % Username       = proplists:get_value(username, Options, undefined),
    State = #state{host            = Host,
                   port            = Port,
                   password        = list_to_binary(Password),
                   reconnect_sleep = ?RECONNECT_SLEEP,
                   channels        = [],
                   parser_state    = eredis_parser:init(),
                   auth_cmd        = get_auth_command(undefined, Password),
                   msg_queue       = queue:new(),
                   max_queue_size  = MaxQueueSize,
                   queue_behaviour = QueueBehaviour,
                   tls_options     = TlsOptions,
                   socket          = undefined,
                   connect_timeout = ?CONNECT_TIMEOUT,
                   socket_options  = [],
                   transport       = Transport},

    case connect(State) of
        {ok, NewState} ->
            {ok, NewState};
        {error, Reason} ->
            {stop, Reason}
    end.

%% Set the controlling process. All messages on all channels are directed here.
handle_call({controlling_process, Pid}, _From, State) ->
    case State#state.controlling_process of
        undefined ->
            ok;
        {OldRef, _OldPid} ->
            erlang:demonitor(OldRef)
    end,
    Ref = erlang:monitor(process, Pid),
    {reply, ok, State#state{controlling_process={Ref, Pid}, msg_state = ready}};

handle_call(get_channels, _From, State) ->
    {reply, {ok, State#state.channels}, State};


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.


%% Controlling process acks, but we have no connection. When the
%% connection comes back up, we should be ready to forward a message
%% again.
handle_cast({ack_message, Pid},
            #state{controlling_process={_, Pid}, socket = undefined} = State) ->
    {noreply, State#state{msg_state = ready}};

%% Controlling process acknowledges receipt of previous message. Send
%% the next if there is any messages queued or ask for more on the
%% socket.
handle_cast({ack_message, Pid},
            #state{controlling_process={_, Pid}} = State) ->
    NewState = case queue:out(State#state.msg_queue) of
                   {empty, _Queue} ->
                       State#state{msg_state = ready};
                   {{value, Msg}, Queue} ->
                       send_to_controller(Msg, State),
                       State#state{msg_queue = Queue, msg_state = need_ack}
               end,
    {noreply, NewState};

handle_cast({subscribe, Pid, Channels}, #state{controlling_process = {_, Pid}} = State) ->
    Command = eredis:create_multibulk(["SUBSCRIBE" | Channels]),
    ok = gen_tcp:send(State#state.socket, Command),
    NewChannels = add_channels(Channels, State#state.channels),
    {noreply, State#state{channels = NewChannels}};


handle_cast({psubscribe, Pid, Channels}, #state{controlling_process = {_, Pid}} = State) ->
    Command = eredis:create_multibulk(["PSUBSCRIBE" | Channels]),
    case State#state.socket of
        {_,{_,_,_,_},_}  -> ssl:send(State#state.socket, Command);
        _ -> gen_tcp:send(State#state.socket, Command)
    end, 

    NewChannels = add_channels(Channels, State#state.channels),
    {noreply, State#state{channels = NewChannels}};



handle_cast({unsubscribe, Pid, Channels}, #state{controlling_process = {_, Pid}} = State) ->
    Command = eredis:create_multibulk(["UNSUBSCRIBE" | Channels]),
    ok = gen_tcp:send(State#state.socket, Command),
    NewChannels = remove_channels(Channels, State#state.channels),
    {noreply, State#state{channels = NewChannels}};



handle_cast({punsubscribe, Pid, Channels}, #state{controlling_process = {_, Pid}} = State) ->
    Command = eredis:create_multibulk(["PUNSUBSCRIBE" | Channels]),
    ok = gen_tcp:send(State#state.socket, Command),
    NewChannels = remove_channels(Channels, State#state.channels),
    {noreply, State#state{channels = NewChannels}};



handle_cast({ack_message, _}, State) ->
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


%% Receive data from socket, see handle_response/2
handle_info({Type, _Socket, Bs}, State) when Type =:= tcp orelse Type =:= ssl ->
    
     case State#state.socket of
        {_,{_,_,_,_},_}  -> ssl:setopts(State#state.socket, [{active, once}]);
        _ -> inet:setopts(tls_socket(State#state.socket), [{active, once}])
    end, 
    
    NewState = handle_response(Bs, State),
    case queue:len(NewState#state.msg_queue) > NewState#state.max_queue_size of
        true ->
            case State#state.queue_behaviour of
                drop ->
                    Msg = {dropped, queue:len(NewState#state.msg_queue)},
                    send_to_controller(Msg, NewState),
                    {noreply, NewState#state{msg_queue = queue:new()}};
                exit ->
                    {stop, max_queue_size, State}
            end;
        false ->
            {noreply, NewState}
    end;

handle_info({tcp_error, _Socket, _Reason}, State) ->
    %% This will be followed by a close
    {noreply, State};

%% Socket got closed, for example by Redis terminating idle
%% clients. If desired, spawn of a new process which will try to reconnect and
%% notify us when Redis is ready. In the meantime, we can respond with
%% an error message to all our clients.
handle_info({Type, Socket}, #state{socket = OurSocket} = State)
  when OurSocket =/= Socket,
       Type =:= tcp_closed orelse Type =:= ssl_closed ->
    {noreply, State};

handle_info({Type, _Socket}, #state{reconnect_sleep = no_reconnect} = State) ->
    %% If we aren't going to reconnect, then there is nothing else for this process to do.
    {stop, normal, State#state{socket = undefined}};

handle_info({Type, _Socket}, State) ->
    Self = self(),
    send_to_controller({eredis_disconnected, Self}, State),
    spawn(fun() -> reconnect_loop(Self, State) end),

    %% Throw away the socket. The absence of a socket is used to
    %% signal we are "down"; discard possibly patrially parsed data
    {noreply, State#state{socket = undefined, parser_state = eredis_parser:init()}};

%% Controller might want to be notified about every reconnect attempt
handle_info(reconnect_attempt, State) ->
    send_to_controller({eredis_reconnect_attempt, self()}, State),
    {noreply, State};

%% Controller might want to be notified about every reconnect failure and reason
handle_info({reconnect_failed, Reason}, State) ->
    send_to_controller({eredis_reconnect_failed, self(),
                        {error, {connection_error, Reason}}}, State),
    {noreply, State};

%% Redis is ready to accept requests, the given Socket is a socket
%% already connected and authenticated.
handle_info({connection_ready, Socket}, #state{socket = undefined} = State) ->
    send_to_controller({eredis_connected, self()}, State),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State#state{socket = Socket}};


%% Our controlling process is down.
handle_info({'DOWN', Ref, process, Pid, _Reason},
            #state{controlling_process={Ref, Pid}} = State) ->
    {stop, shutdown, State#state{controlling_process=undefined,
                                 msg_state=ready,
                                 msg_queue=queue:new()}};

%% eredis can be used in Poolboy, but it requires to support a simple API
%% that Poolboy uses to manage the connections.
handle_info(stop, State) ->
    {stop, shutdown, State};

handle_info(Info, State) ->
    {stop, {unhandled_message, Info}, State}.

terminate(_Reason, State) ->
    case State#state.socket of
        undefined -> ok;
        {_,{_,_,_,_},_} -> ssl:close(State#state.socket);
        Socket    -> gen_tcp:close(Socket)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec remove_channels([binary()], [binary()]) -> [binary()].
remove_channels(Channels, OldChannels) ->
    lists:foldl(fun lists:delete/2, OldChannels, Channels).

-spec add_channels([binary()], [binary()]) -> [binary()].
add_channels(Channels, OldChannels) ->
    lists:foldl(fun(C, Cs) ->
        case lists:member(C, Cs) of
            true ->
                Cs;
            false ->
                [C|Cs]
        end
    end, OldChannels, Channels).

-spec handle_response(Data::binary(), State::#state{}) -> NewState::#state{}.
%% @doc: Handle the response coming from Redis. This should only be
%% channel messages that we should forward to the controlling process
%% or queue if the previous message has not been acked. If there are
%% more than a single response in the data we got, queue the responses
%% and serve them up when the controlling process is ready
handle_response(Data, #state{parser_state = ParserState} = State) ->
    case eredis_parser:parse(ParserState, Data) of
        {ReturnCode, Value, NewParserState} ->
            reply({ReturnCode, Value},
                  State#state{parser_state=NewParserState});

        {ReturnCode, Value, Rest, NewParserState} ->
            NewState = reply({ReturnCode, Value},
                             State#state{parser_state=NewParserState}),
            handle_response(Rest, NewState);

        {continue, NewParserState} ->
            State#state{parser_state = NewParserState}
    end.

%% @doc: Sends a reply to the controlling process if the process has
%% acknowledged the previous process, otherwise the message is queued
%% for later delivery.
reply({ok, [<<"message">>, Channel, Message]}, State) ->
    queue_or_send({message, Channel, Message, self()}, State);

reply({ok, [<<"pmessage">>, Pattern, Channel, Message]}, State) ->
    queue_or_send({pmessage, Pattern, Channel, Message, self()}, State);



reply({ok, [<<"subscribe">>, Channel, _]}, State) ->
    queue_or_send({subscribed, Channel, self()}, State);

reply({ok, [<<"psubscribe">>, Channel, _]}, State) ->
    queue_or_send({subscribed, Channel, self()}, State);


reply({ok, [<<"unsubscribe">>, Channel, _]}, State) ->
    queue_or_send({unsubscribed, Channel, self()}, State);


reply({ok, [<<"punsubscribe">>, Channel, _]}, State) ->
    queue_or_send({unsubscribed, Channel, self()}, State);
reply({ReturnCode, Value}, State) ->
    throw({unexpected_response_from_redis, ReturnCode, Value, State}).


queue_or_send(Msg, State) ->
    case State#state.msg_state of
        need_ack ->
            MsgQueue = queue:in(Msg, State#state.msg_queue),
            State#state{msg_queue = MsgQueue};
        ready ->
            send_to_controller(Msg, State),
            State#state{msg_state = need_ack}
    end.


%% @doc: Helper for connecting to Redis. These commands are
%% synchronous and if Redis returns something we don't expect, we
%% crash. Returns {ok, State} or {error, Reason}.
connect(State) ->
    {ok, {AFamily, Addrs}} = get_addrs(State#state.host),
    Port = case AFamily of
               local -> 0;
               _ -> State#state.port
           end,

    SocketOptions = lists:ukeymerge(1, lists:keysort(1, State#state.socket_options),
                                    lists:keysort(1, ?SOCKET_OPTS)),
    ConnectOptions = [AFamily | [?SOCKET_MODE | SocketOptions]],

    connect_next_addr(Addrs, Port, ConnectOptions, State).

connect_next_addr([Addr|Addrs], Port, ConnectOptions, State) ->
    case gen_tcp:connect(Addr, Port, ConnectOptions, State#state.connect_timeout) of
        {ok, Socket} ->
            case maybe_upgrade_to_tls(Socket, State) of
                {ok, NewSocket} ->
                    case authenticate(NewSocket, State#state.transport,
                                      State#state.auth_cmd) of
                        ok ->
                            case select_database(NewSocket, State#state.transport, State#state.database) of
                                ok ->
                                    % S = tls_socket(NewSocket),
                                    {ok, State#state{socket = NewSocket}};
                                {error, Reason} ->
                                    close_socket(State, NewSocket),
                                    {error, {select_error, Reason}}
                            end;
                        {error, Reason} ->
                            close_socket(State, NewSocket),
                            {error, {authentication_error, Reason}}
                    end;
                {error, Reason} ->
                    gen_tcp:close(Socket),
                    {error, {failed_to_upgrade_to_tls, Reason}} %% Used in TLS v1.2
            end;
        {error, Reason} when Addrs =:= [] ->
            {error, {connection_error, Reason}};
        {error, _Reason} ->
            %% Try next address
            connect_next_addr(Addrs, Port, ConnectOptions, State)
    end.


% authenticate(_Socket, <<>>) ->
%     ok;
% authenticate(Socket, Password) ->
%     eredis_client:do_sync_command(Socket, gen_tcp, ["AUTH", " \"", Password, "\"\r\n"]).
% authenticate(Socket, Password) ->
%     eredis_client:do_sync_command(Socket, gen_tcp, ["AUTH", " \"", Password, "\"\r\n"]).


%% @doc: Loop until a connection can be established, this includes
%% successfully issuing the auth and select calls. When we have a
%% connection, give the socket to the redis client.
reconnect_loop(Client, #state{reconnect_sleep=ReconnectSleep}=State) ->
    Client ! reconnect_attempt,
    case catch(connect(State)) of
        {ok, #state{socket = Socket}} ->
            gen_tcp:controlling_process(Socket, Client),
            Client ! {connection_ready, Socket};
        {error, Reason} ->
            Client ! {reconnect_failed, Reason},
            timer:sleep(ReconnectSleep),
            reconnect_loop(Client, State);
        %% Something bad happened when connecting, like Redis might be
        %% loading the dataset and we got something other than 'OK' in
        %% auth or select
        _ ->
            timer:sleep(ReconnectSleep),
            reconnect_loop(Client, State)
    end.


send_to_controller(_Msg, #state{controlling_process=undefined}) ->
    ok;
send_to_controller(Msg, #state{controlling_process={_Ref, Pid}}) ->
    Pid ! Msg.



maybe_upgrade_to_tls(Socket, #state{transport = ssl} = State) ->
    %% setopt needs to be 'false' before an upgrade to ssl is possible
    case inet:setopts(Socket, [{active, false}]) of
        ok ->
            upgrade_to_tls(Socket, State);
        {error, Reason} ->
            {error, Reason}
    end;
maybe_upgrade_to_tls(Socket, _State) ->
    {ok, Socket}.


upgrade_to_tls(Socket, State) ->
    case ssl:connect(Socket, State#state.tls_options, State#state.connect_timeout) of
        {ok, NewSocket} ->
            %% Enter `{active, once}' mode. NOTE: tls/ssl doesn't support `{active, N}'
            case ssl:setopts(NewSocket, [{active, once}]) of
                ok ->
                    {ok, NewSocket};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.



    get_addrs({local, Path}) ->
        {ok, {local, [{local, Path}]}};
    get_addrs(Hostname) ->
        case inet:parse_address(Hostname) of
            {ok, {_, _, _, _} = Addr} ->             {ok, {inet, [Addr]}};
            {ok, {_, _, _, _, _, _, _, _} = Addr} -> {ok, {inet6, [Addr]}};
            {error, einval} ->
                case inet:getaddrs(Hostname, inet6) of
                    {error, _} ->
                        case inet:getaddrs(Hostname, inet) of
                            {ok, Addrs} ->
                                {ok, {inet, deduplicate(Addrs)}};
                            {error, _} = Res ->
                                Res
                        end;
                    {ok, Addrs} ->
                        {ok, {inet6, deduplicate(Addrs)}}
                end
        end.


%% Removes duplicates without sorting.
deduplicate([X|Xs]) ->
    [X | deduplicate([Y || Y <- Xs,
                           Y =/= X])];
deduplicate([]) ->
    [].

select_database(_Socket, _TransportType, undefined) ->
    ok;
select_database(_Socket, _TransportType, <<"0">>) ->
    ok;
select_database(Socket, TransportType, Database) ->
    do_sync_command(Socket, TransportType, ["SELECT", " ", Database, "\r\n"]).

authenticate(_Socket, _TransportType, undefined) ->
    ok;
authenticate(Socket, TransportType, AuthCmd) ->
    do_sync_command(Socket, TransportType, AuthCmd).

%% @doc: Executes the given command synchronously, expects Redis to
%% return "+OK\r\n", otherwise it will fail.
do_sync_command(Socket, Transport, Command) ->
    case setopts(Socket, Transport, [{active, false}]) of
        ok ->
            do_sync_command2(Socket, Transport, Command);
        {error, Reason} ->
            {error, Reason}
    end.

do_sync_command2(Socket, Transport, Command) ->
    case Transport:send(Socket, Command) of
        ok ->
            case Transport:recv(Socket, 0, ?RECV_TIMEOUT) of
                {ok, <<"+OK\r\n">>} ->
                    setopts(Socket, Transport, [{active, once}]);
                {ok, Data} ->
                    {error, {unexpected_response, Data}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

close_socket(_State, _Socket = undefined) -> ok;
close_socket(#state{transport = ssl}, Socket) -> ssl:close(Socket);
close_socket(#state{transport = gen_tcp}, Socket) -> gen_tcp:close(Socket).

setopts(Socket, _Transport=gen_tcp, Opts) -> inet:setopts(Socket, Opts);
setopts(Socket, _Transport=ssl, Opts)     ->  ssl:setopts(Socket, Opts).

maybe_reconnect(Reason, #state{reconnect_sleep = no_reconnect, queue = Queue} = State) ->
    reply_all({error, Reason}, Queue),
    %% If we aren't going to reconnect, then there is nothing else for
    %% this process to do.
    {stop, normal, State#state{socket = undefined}};
maybe_reconnect(Reason, #state{queue = Queue} = State) ->
    error_logger:error_msg("eredis: Re-establishing connection to ~p:~p due to ~p",
                           [State#state.host, State#state.port, Reason]),
    Self = self(),
    spawn_link(fun() -> process_flag(trap_exit, true),
                        reconnect_loop(Self, State)
               end),

    %% tell all of our clients what has happened.
    reply_all({error, Reason}, Queue),

    %% Throw away the socket and the queue, as we will never get a
    %% response to the requests sent on the old socket. The absence of
    %% a socket is used to signal we are "down"
    {noreply, State#state{socket = undefined, queue = queue:new()}}.

%% @doc: Loop until a connection can be established, this includes
%% successfully issuing the auth and select calls. When we have a
%% connection, give the socket to the redis client.


read_database(undefined) ->
    undefined;
read_database(Database) when is_integer(Database) ->
    list_to_binary(integer_to_list(Database)).

-spec get_auth_command(Username :: iodata() | undefined,
                       Password :: iodata() | undefined) ->
          iodata() | undefined.
get_auth_command(undefined, undefined) ->
    undefined;
get_auth_command(undefined, "") -> % legacy
    undefined;
get_auth_command(undefined, Password) ->
    eredis:create_multibulk([<<"AUTH">>, Password]);
get_auth_command(Username, Password) ->
    eredis:create_multibulk([<<"AUTH">>, Username, Password]).

get_all_messages(Acc) ->
    receive
        M ->
            get_all_messages([M | Acc])
    after 0 ->
            lists:reverse(Acc)
    end.
    reply_all(Value, Queue) ->
        case queue:peek(Queue) of
            empty ->
                ok;
            {value, Item} ->
                safe_reply(receipient(Item), Value),
                reply_all(Value, queue:drop(Queue))
        end.

safe_reply(undefined, _Value) ->
    ok;
safe_reply(Pid, Value) when is_pid(Pid) ->
    safe_send(Pid, {response, Value});
safe_reply(From, Value) ->
    gen_server:reply(From, Value).

safe_send(Pid, Value) ->
    try erlang:send(Pid, Value)
    catch
        Err:Reason ->
            error_logger:info_msg("eredis: Failed to send message to ~p with reason ~p~n", [Pid, {Err, Reason}])
    end.


receipient({_, From}) ->
    From;
receipient({_, From, _}) ->
    From.


tls_socket({_,{_,Port,_,_},_}) -> Port;
tls_socket(Port) -> Port.