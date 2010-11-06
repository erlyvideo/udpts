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

-export([test/0, reload/0, start_reader/2]).

start() ->
  Config = case file:path_consult(["priv", "/etc/udpts"], "udpts.conf") of
    {ok, Env, _Path} ->
      Env;
    _ ->
      [{udp_listeners, [{5670,"vlc"}]},{http_port,8000}]
  end,
  application:start(udpts),
  [udpts:start_reader(Port, Name) || {Port,Name} <- proplists:get_value(udp_listeners, Config, [])],
  SC = [{port,proplists:get_value(http_port, Config)}, {appmods,[{"/stream",udpts_http}]}],
  GC = [{enable_soap,false},{flags,[{auth_log,false},{logdir,"log"}]}],
  yaws:start_embedded("wwwroot", SC, GC, "udpts_httpd"),
  ok.
  
  
  
start_reader(Port, Name) ->
  udpts_sup:start_reader(Port, Name).


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

