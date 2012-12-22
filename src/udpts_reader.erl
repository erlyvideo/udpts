%%% @author     Max Lapshin <max@erlyvideo.org> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        UDP TS reader
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%---------------------------------------------------------------------------------------
-module(udpts_reader).
-author('Max Lapshin <max@erlyvideo.org>').
-behaviour(gen_server).
-include("udpts.hrl").

%% External API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-export([subscribe/2]).


start_link(Port, Name, Options) ->
  gen_server:start_link(?MODULE, [Port, Name, Options], []).


subscribe(Name, Socket) ->
  case ets:lookup(udpts_streams, Name) of
    [#stream{name = Name, pid = Pid}] ->
      erlang:monitor(process, Pid),
      gen_server:call(Pid, {subscribe, self(), Socket}, 10000);
    [] -> 
      {error, enoent}
  end.
  
-record(reader, {
  socket,
  name,
  port,
  seen,
  multicast,
  clients
}).

-record(udp_client, {
  pid,
  socket
}).


%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------


init([Port, Name, Options]) ->
  {ok, Socket} = mpegts_udp:open(Port, Options),
  mpegts_udp:active_once(Socket),
      
  error_logger:info_msg("UDP Listener bound to port: ~p", [Port]),
  ets:insert(udpts_streams, #stream{name = Name, pid = self(), port = Port, multicast = proplists:get_value(mc, Options, ""), last_packet_at = {0,0,0}}),
  timer:send_interval(proplists:get_value(error_flush_timeout, Options, 60000), flush_errors),
  Clients = ets:new(clients, [private,{keypos,#udp_client.pid}]),
  {ok, #reader{socket = Socket, port = Port, name = Name, clients = Clients}}.




%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call({subscribe, Client, Socket}, _From, #reader{name = Name, clients = Clients} = Reader) ->
  erlang:monitor(process, Client),
  gen_tcp:send(Socket, "HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Type: video/mpeg2\r\n\r\n"),
  ets:update_counter(udpts_streams, Name, {#stream.clients_count, 1}),
  ets:insert(Clients, #udp_client{pid = Client, socket = Socket}),
  {reply, {ok, self()}, Reader};
  
handle_call(Request, _From, State) ->
  {stop, {unknown_call, Request}, State}.


%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info({'DOWN', _, process, Client, _Reason}, #reader{name = Name, clients = Clients} = Reader) ->
  ets:update_counter(udpts_streams, Name, {#stream.clients_count, -1}),
  ets:delete(Clients, Client),
  {noreply, Reader};


handle_info({mpegts_udp, Socket, Data}, #reader{socket = Socket, name = Name, seen = Seen} = Reader) ->
  mpegts_udp:active_once(Socket),

  Reader1 = case Seen of 
    undefined -> error_logger:info_msg("First data for ~s~n", [Name]), Reader#reader{seen = true};
    _ -> Reader 
  end,
  ets:update_element(udpts_streams, Name, {#stream.last_packet_at, os:timestamp()}),
  {noreply, handle_ts(Data, Reader1)};

handle_info(flush_errors, #reader{socket = Socket, name = Name} = Reader) ->
  Errors = mpegts_udp:control(Socket, errors),
  PacketCount = mpegts_udp:control(Socket, packet_count),
  ets:update_element(udpts_streams, Name, [{#stream.errors_count, Errors},{#stream.packets_count, PacketCount}]),
  ScrambledCount = mpegts_udp:control(Socket, scrambled),
  if ScrambledCount > PacketCount div 10 -> ets:update_element(udpts_streams, Name, {#stream.scrambled, true});
    true -> ets:update_element(udpts_streams, Name, {#stream.scrambled, false})
  end,
  {noreply, Reader};

handle_info({inet_reply, _Client, _Reply}, Reader) ->
  {noreply, Reader};

handle_info(_Info, State) ->
  ?D({unknown_message, _Info}),
  {stop, {unknown_message, _Info}, State}.


%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, #reader{name = Name}) ->
  ets:delete(udpts_streams, Name),
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



handle_ts(Packet, #reader{clients = Clients} = Reader) ->
  Removing = ets:foldl(fun(#udp_client{pid = Pid, socket = Socket}, Acc) ->
    case (catch port_command(Socket, Packet,[nosuspend])) of
      true -> Acc;
      {'EXIT', _} -> [Pid|Acc];
      false -> [Pid|Acc]
    end
  end, [], Clients),
  [Pid ! stop || Pid <- Removing],
  Reader.





