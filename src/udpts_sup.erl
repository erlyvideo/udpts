%%% @author     Max Lapshin <max@erlyvideo.org> [http://erlyvideo.org]
%%% @copyright  2010 Max Lapshin
%%% @doc        Worker module for plugin example
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%%---------------------------------------------------------------------------------------
-module(udpts_sup).
-author('Max Lapshin <max@erlyvideo.org>').
-include("udpts.hrl").

-behaviour(supervisor).

-export([init/1,start_link/0]).

-export([start_reader/3, stop_reader/1, start_http_listener/1, stop_http_listener/1, start_http_worker/0]).
-define(NAMED_SERVER(Id,M,A), {Id,                               % Id       = internal id
    {M,start_link,A},                  % StartFun = {M, F, A}
    temporary,                               % Restart  = permanent | transient | temporary
    2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
    worker,                                  % Type     = worker | supervisor
    [M]                                      % Modules  = [Module] | dynamic
}).

-define(SIMPLE_SERVER(M,A), ?NAMED_SERVER(undefined, M, A)).

-define(SUPERVISOR_LINK(Name), {Name,
    {supervisor,start_link,[{local, Name}, ?MODULE, [Name]]},
    permanent,                               % Restart  = permanent | transient | temporary
    infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
    supervisor,                              % Type     = worker | supervisor
    []                                       % Modules  = [Module] | dynamic
}).


%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for whole supervisor. Started by application
%% @end 
%%--------------------------------------------------------------------
-spec start_link() -> {'error',_} | {'ok',pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_http_listener(Port) ->
  Id = {udpts_http,Port},
  Reader = 
  { 
    Id,
    {gen_listener, start_link, [Port, udpts_http, []]},
    transient,
    10000,
    worker,
    [udpts_http]
  },
  supervisor:start_child(?MODULE, Reader).

stop_http_listener(Port) ->
  Id = {udpts_http,Port},
  supervisor:terminate_child(?MODULE, Id),
  supervisor:delete_child(?MODULE, Id).

start_http_worker() ->
  supervisor:start_child(http_worker_sup, []).
  

reader_name(Port) when is_integer(Port) ->
  "udpts_reader:"++integer_to_list(Port);
reader_name(Name) when is_list(Name) ->
  "udpts_reader:"++Name.

start_reader(Port, Name, Options) ->
  Id = reader_name(Name),
  Reader = 
  { 
    Id,
    {udpts_reader, start_link, [Port, Name, Options]},
    transient,
    10000,
    worker,
    [udpts_reader]
  },
  supervisor:start_child(?MODULE, Reader).

stop_reader(Name) ->
  Id = reader_name(Name),
  supervisor:terminate_child(?MODULE, Id),
  supervisor:delete_child(?MODULE, Id).

init([http_worker_sup]) ->
  {ok, {{simple_one_for_one, 100, 100}, [?SIMPLE_SERVER(udpts_http, [])]}};


init([]) ->
  udpts_streams = ets:new(udpts_streams,[set,public,named_table,{keypos, #stream.name}]),
  
  Supervisors = [
    {
      udpts_config_sup,
      {udpts_config, start_link, []},
      permanent,
      1000,
      worker,
      [udpts_config]
    },
    ?SUPERVISOR_LINK(http_worker_sup)
  ],
  
  {ok, {{one_for_one, 2, 2}, Supervisors}}.
