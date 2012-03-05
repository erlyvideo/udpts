%%% @author     Max Lapshin <max@erlyvideo.org> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        Worker module for plugin example
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%%---------------------------------------------------------------------------------------
-module(udpts).
-author('Max Lapshin <max@erlyvideo.org>').
-behaviour(application).
-include("udpts.hrl").

% Application API
-export([start/2, stop/1, config_change/3]).


% PLUGIN API
-export([start/0, stop/0]).

-export([test/0, reload/0, start_reader/3, stop_reader/1]).

start() ->
  application:start(udpts),
  write_pid(),
  udpts_config:load(),
  ok.


write_pid() ->
  Path = case os:getenv("PID_PATH") of
    false -> "log/flussonic.pid";
    PidPath -> PidPath
  end,
  filelib:ensure_dir(Path),
  file:write_file(Path, os:getpid()).
  
  
  
start_reader(Name, Port, Options) ->
  case ets:lookup(udpts_streams, Name) of
    [] ->
      error_logger:info_msg("Start UDP reader ~s on group ~s:~p", [Name, proplists:get_value(mc, Options, ""), Port]),
      udpts_sup:start_reader(Port, Name, Options);
    [_] ->
      error_logger:error_msg("Cant add UDP reader ~s because it is already started", [Name]),
      {error, duplicate}
  end.    

stop_reader(Name) ->
  error_logger:info_msg("Stop reader ~s", [Name]),
  udpts_sup:stop_reader(Name).

test() ->
  ok.


reload() ->
  {ok, Modules} = application:get_key(udpts,modules),
  make:all(),
  [begin
    code:soft_purge(Module),
    code:purge(Module),
    code:load_file(Module)
  end || Module <- Modules].


  
%%--------------------------------------------------------------------
%% @spec (Type::any(), Args::list()) -> any()
%% @doc Starts RTMP library
%% @end 
%%--------------------------------------------------------------------

start(_Type, _Args) -> 
  udpts_sup:start_link().



%%--------------------------------------------------------------------
%% @spec (Any::any()) -> ok()
%% @doc Stop RTMP library
%% @end 
%%--------------------------------------------------------------------
stop(_S) ->
  ok.


%%--------------------------------------------------------------------
%% @spec (Any::any(),Any::any(),Any::any()) -> any()
%% @doc Reload ErlMedia Application config
%% @end 
%%--------------------------------------------------------------------
config_change(_Changed, _New, _Remove) ->
  ok.
  
stop() -> 
  application:stop(udpts),
  application:unload(udpts).

