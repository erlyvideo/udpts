%%% @author     Max Lapshin <max@erlyvideo.org> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        UDP TS reader
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% /Applications/VLC.app/Contents/MacOS/VLC -vvvv ~/Movies/transformers.mp4 --sout '#std{mux=ts,access=udp,dts=127.0.0.1:5670}'
%%% /Applications/VLC.app/Contents/MacOS/VLC -vvvv http://localhost:8000/stream/vlc1
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
    [{Name,Pid}] ->
      erlang:monitor(process, Pid),
      gen_server:call(Pid, {subscribe, self(), Socket}, 10000);
    [] -> 
      {error, enoent}
  end.
  
-record(reader, {
  socket,
  name,
  port,
  clients = [],
  buffer = <<>>,
  errors = [],
  counters,
  error_count = 0,
  desync_count = 0
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
  {ok, Socket} = gen_udp:open(Port, [binary, {active,true},{recbuf,65536},inet,{ip,{0,0,0,0}}]),
  error_logger:info_msg("UDP Listener bound to port: ~p", [Port]),
  erlang:process_flag(trap_exit, true),
  ets:insert(udpts_streams, {Name, self()}),
  Counters = hipe_bifs:bytearray(8192, 16#FF),  %% MPEG-TS counters take 13 bits. it is 8192 maximum
  timer:send_interval(proplists:get_value(error_flush_timeout, Options, 60000), flush_errors),
  {ok, #reader{socket = Socket, port = Port, name = Name, counters = Counters}}.

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
handle_call({subscribe, Client, Socket}, _From, #reader{clients = Clients} = Reader) ->
  erlang:monitor(process, Client),
  gen_tcp:send(Socket, "HTTP/1.1 200 OK\r\nContent-Type: video/mpeg2\r\n\r\n"),
  {reply, {ok, self()}, Reader#reader{clients = [{Client,Socket}|Clients]}};
  
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
handle_info({'DOWN', _, process, Client, _Reason}, #reader{clients = Clients} = Reader) ->
  {noreply, Reader#reader{clients = lists:keydelete(Client, 1, Clients)}};

handle_info({udp, Socket, _IP, _InPortNo, Packet}, Reader) ->
  Data = flush_udp_packets(Socket, [Packet]),
  % ?D({udp, size(Packet)}),
  % inet:setopts(Socket, [{active, once}]),
  {noreply, handle_packet(Data, Reader)};

handle_info(flush_errors, #reader{error_count = 0} = Reader) -> 
  {noreply, Reader#reader{error_count = 0}};

handle_info(flush_errors, #reader{error_count = ErrorCount, desync_count = DesyncCount, name = Name, port = Port} = Reader) ->
  % Errors = case length(Reader#reader.errors) of
  %   L when L > 20 -> lists:reverse(lists:nthtail(20, Reader#reader.errors));
  %   _ -> lists:reverse(Reader#reader.errors)
  % end,
  % error_logger:info_msg("~p ~p error_count: ~p, desync_count: ~p, errors: ~p", [Name, Port, ErrorCount, DesyncCount, Errors]),
  error_logger:info_msg("~p ~p error_count: ~p, desync_count: ~p", [Name, Port, ErrorCount, DesyncCount]),
  {noreply, Reader#reader{error_count = 0, desync_count = 0}};


handle_info(_Info, State) ->
  ?D({unknown_message, _Info}),
  {stop, {unknown_message, _Info}, State}.


flush_udp_packets(Socket, Packets) when length(Packets) < 200 ->
  receive
    {udp, Socket, _IP, _InPortNo, Packet} -> flush_udp_packets(Socket, [Packet|Packets])
  after
    0 -> 
      inet:setopts(Socket, [{active,true}]),
      iolist_to_binary(lists:reverse(Packets))
  end;
  
flush_udp_packets(Socket, Packets) ->
  inet:setopts(Socket, [{active,once}]),
  iolist_to_binary(lists:reverse(Packets)).
    

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


get_counter(#reader{counters = Counters}, Pid) ->
  hipe_bifs:bytearray_sub(Counters, Pid).
  
set_counter(#reader{counters = Counters} = Reader, Pid, Counter) ->
  hipe_bifs:bytearray_update(Counters, Pid, Counter),
  Reader.
  


handle_packet(Packet, #reader{buffer = Buf} = Reader) when size(Buf) == 0 ->
  handle_ts(Packet, Reader);

handle_packet(Packet, #reader{buffer = Buf} = Reader) ->
  handle_ts(<<Buf/binary, Packet/binary>>, Reader).
  
handle_ts(Packet, #reader{} = Reader) ->
  sync_packet(Packet, Reader).

sync_packet(<<16#47, _:187/binary, 16#47, _:187/binary, 16#47, _/binary>> = Packet, Reader) ->
  {Packet1, _Errors, Reader1} = verify_ts(Packet, Reader, [], []),
  % case Errors of
  %   [] -> ok;
  %   _ -> error_logger:error_msg("Errors: ~p", [Errors])
  % end,
  send_packet(Packet1, Reader1);
  
sync_packet(<<_, Packet/binary>> = AllPacket, #reader{desync_count = DesyncCount} = Reader) when size(AllPacket) > 377 ->
  sync_packet(Packet, Reader#reader{desync_count = DesyncCount + 1});

sync_packet(Packet, #reader{} = Reader) ->
  Reader#reader{buffer = Packet}.
  

  
verify_ts(<<Packet:188/binary, Rest/binary>>, Reader, Errors, Output) ->
  <<16#47, _TEI:1, _Start:1, _:1, Pid:13, _Opt:4, Counter:4, _:184/binary>> = Packet,
  WaitFor = (Counter - 1 + 16) rem 16,
  case get_counter(Reader, Pid) of
    WaitFor ->
      verify_ts(Rest, set_counter(Reader, Pid, Counter), Errors, [Packet|Output]);
    16#FF ->
      verify_ts(Rest, set_counter(Reader, Pid, Counter), Errors, [Packet|Output]);
    Counter ->
      verify_ts(Rest, set_counter(Reader, Pid, Counter), Errors, Output);
    Else ->
      verify_ts(Rest, set_counter(Reader, Pid, Counter), [{ts_counter,Pid,Else,Counter}|Errors], [Packet|Output])
  end;
    
verify_ts(Rest, #reader{error_count = ErrorCount} = Reader, Errors, Output) ->
  {lists:reverse(Output), Errors, Reader#reader{buffer = Rest, error_count = ErrorCount + length(Errors)}}.


  

send_packet(Packet, #reader{clients = Clients} = Reader) ->
  [gen_tcp:send(Socket, Packet) || {_Pid,Socket} <- Clients],
  Reader.





