-module(udpts_config).

-export([start_link/0, load/0, unload/0]).
-export([init/1, handle_info/2, terminate/2]).
-include_lib("kernel/include/file.hrl").

-record(conf, {
  
}).


start_link() ->
  gen_server:start_link(?MODULE, [], []).


load() ->
  {ok, Config, RealPath} = file:path_consult(["priv", "/etc/udpts"], "udpts.conf"),
  error_logger:info_msg("Reading config from ~s: ~n~p", [RealPath, Config]),
  Options = [{error_flush_timeout, proplists:get_value(error_flush_timeout, Config, 60000)}],
  io:format("Listeners: ~p~n", [proplists:get_value(udp_listeners, Config)]),
  
  lists:foreach(fun
    ({Port,Name}) ->
      udpts:start_reader(Name, Port, Options);
    ({Multicast,Port,Name}) ->
      udpts:start_reader(Name, Port, [{mc,Multicast}|Options])
  end, proplists:get_value(udp_listeners, Config, [])),
  HTTPPort = proplists:get_value(http_port, Config),
  udpts_sup:start_http_listener(HTTPPort),
  application:set_env(udpts, config, Config),
  application:set_env(udpts, config_path, RealPath),
  {ok, #file_info{mtime = Mtime}} = file:read_file_info(RealPath),
  application:set_env(udpts, config_mtime, Mtime),
  RealPath.


unload() ->
  [udpts_sup:stop_reader(Id) || {"udpts_reader:"++Id, _, worker, [udpts_reader]} <- supervisor:which_children(udpts_sup)],
  [udpts_sup:stop_http_listener(Port) || {{udpts_http,Port}, _, worker, [udpts_http]} <- supervisor:which_children(udpts_sup)],
  ok.

check_mtime() ->
  {ok, RealPath} = application:get_env(udpts, config_path),
  {ok, OldMtime} = application:get_env(udpts, config_mtime),
  {ok, #file_info{mtime = Mtime}} = file:read_file_info(RealPath),
  if Mtime == OldMtime -> ok;
  true ->
    error_logger:info_msg("Reloading config from ~s~n", [RealPath]),
    unload(),
    load()
  end.  
  

init([]) ->
  timer:send_interval(2000, self(), check),
  {ok, #conf{}}.

handle_info(check, State) ->
  check_mtime(),
  {noreply, State}.

terminate(_,_) -> ok.

