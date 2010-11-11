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
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-export([subscribe/2]).


start_link(Port, Name) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, Name], []).


subscribe(Name, Socket) ->
  case ets:lookup(udpts_streams, Name) of
    [{Name,Pid}] ->
      erlang:monitor(process, Pid),
      gen_server:call(Pid, {subscribe, self(), Socket});
    [] -> 
      {error, enoent}
  end.
  
-record(reader, {
  socket,
  name,
  port,
  clients = [],
  buffer = <<>>,
  counters
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


init([Port, Name]) ->
  {ok, Socket} = gen_udp:open(Port, [binary, {active,once},{reuseaddr,true},{recbuf,65536},inet]),
  error_logger:info_msg("UDP Listener bound to port: ~p", [Port]),
  erlang:process_flag(trap_exit, true),
  ets:insert(udpts_streams, {Name, self()}),
  Counters = hipe_bifs:bytearray(8192, 16#FF),  %% MPEG-TS counters take 13 bits. it is 8192 maximum
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
  % ?D({udp, size(Packet)}),
  inet:setopts(Socket, [{active, once}]),
  {noreply, handle_packet(Packet, Reader)};

handle_info(_Info, State) ->
  ?D({unknown_message, _Info}),
  {noreply, State}.

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
  {Count, Errors, Reader1} = verify_ts(Packet, Reader, 0, []),
  case Errors of
    [] -> ok;
    _ -> error_logger:error_msg("Errors: ~p", [Errors])
  end,
  {Packet1, More} = erlang:split_binary(Packet, Count*188),
  send_packet(Packet1, Reader1#reader{buffer = More}).

  
verify_ts(<<16#47, _TEI:1, _Start:1, _:1, Pid:13, _Opt:4, Counter:4, _:184/binary, Rest/binary>>, Reader, Count, Errors) ->
  WaitFor = (Counter - 1 + 16) rem 16,
  case get_counter(Reader, Pid) of
    WaitFor ->
      verify_ts(Rest, set_counter(Reader, Pid, Counter), Count + 1, Errors);
    16#FF ->
      verify_ts(Rest, set_counter(Reader, Pid, Counter), Count + 1, Errors);
    Else ->
      verify_ts(Rest, set_counter(Reader, Pid, Counter), Count + 1, [{ts_counter,Pid,Else,Counter}|Errors])
  end;
    
verify_ts(_Rest, Reader, Count, Errors) ->
  {Count, lists:reverse(Errors), Reader}.


  

send_packet(Packet, #reader{clients = Clients} = Reader) ->
  [yaws_api:stream_process_deliver_chunk(Socket, Packet) || {_Pid,Socket} <- Clients],
  Reader.





