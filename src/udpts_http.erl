%%% @author     Max Lapshin <max@erlyvideo.org> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        HTTPD module for UDPTS
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%%---------------------------------------------------------------------------------------
-module(udpts_http).
-author('Max Lapshin <max@erlyvideo.org>').
-include("udpts.hrl").
-behaviour(gen_server).


-export([accept/2, set_socket/2]).

%% External API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state, {
  socket,
  name
}).

accept(Socket, _Args) ->
  {ok, HTTP} = udpts_sup:start_http_worker(),
  gen_tcp:controlling_process(Socket, HTTP),
  udpts_http:set_socket(HTTP, Socket),
  ok.
  



start_link() ->
  gen_server:start_link(?MODULE, [], []).


set_socket(HTTP, Socket) ->
  gen_server:call(HTTP, {set_socket, Socket}).

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


init([]) ->
  erlang:process_flag(trap_exit, true),
  {ok, #state{}}.

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
handle_call({set_socket, Socket}, _From, #state{} = State) ->
  inet:setopts(Socket, [{active,once},{packet,http}]),
  {reply, ok, State#state{socket = Socket}};
  
  
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
handle_info({http, Socket, {http_request, 'GET', {abs_path, Path}, _Version}}, State) ->
  case Path of
    "/stream/"++Stream ->
      inet:setopts(Socket, [{active,once}]),
      {noreply, State#state{name = Stream}};
    "/favicon.ico" ->
      gen_tcp:send(Socket, "HTTP/1.1 404 Not Found\r\n\r\nNot found\r\n"),
      {stop, normal, State};
    "/stats" ->
      gen_tcp:send(Socket, "HTTP/1.1 200 OK\r\n\r\n"),
      gen_tcp:send(Socket, udpts_stats:html()),
      {stop, normal, State};
    _ ->
      {stop, {unhandled_path, Path}, State}
  end;
  
handle_info({http, Socket, {http_header, _, _Key, _, _Value}}, State) ->
  inet:setopts(Socket, [{active,once}]),
  {noreply, State};

handle_info({http, Socket, http_eoh}, #state{name = Name} = State) ->
  {ok, {Addr,_Port}} = inet:peername(Socket),
  ems_network_lag_monitor:watch(self()),
  case udpts_reader:subscribe(Name, Socket) of
    {ok, Pid} ->
      erlang:monitor(process, Pid),
      inet:setopts(Socket, [{active,true}]),
      error_logger:info_msg("200 ~p ~s~n", [Addr, Name]),
      {noreply, State};
    {error, enoent} ->
      gen_tcp:send(Socket, "HTTP/1.1 404 Not Found\r\n\r\nNot found\r\n"),
      error_logger:info_msg("404 ~p ~s~n", [Addr, Name]),
      {stop, normal, State}
  end;

handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};

handle_info({'DOWN', _, process, _Client, _Reason}, Server) ->
  {stop, normal, Server};
  
handle_info(Bin, #state{socket = Socket} = Server) when is_binary(Bin) ->
  gen_tcp:send(Socket, Bin),
  {noreply, Server};

handle_info(_Info, State) ->
  {stop, {unknown_message, _Info}, State}.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
