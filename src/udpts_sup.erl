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

-export([start_reader/2]).

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for whole supervisor. Started by application
%% @end 
%%--------------------------------------------------------------------
-spec start_link() -> {'error',_} | {'ok',pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


reader_name(Port) when is_integer(Port) ->
  "udpts_reader:"++integer_to_list(Port).

start_reader(Port, Consumer) ->
  Id = reader_name(Port),
  Reader = 
  { 
    Id,
    {udpts_reader, start_link ,[Port, Consumer]},
    transient,
    10000,
    worker,
    [udpts_reader]
  },
  supervisor:start_child(?MODULE, Reader).

init([]) ->
  Supervisors = [
  ],
  
  {ok, {{one_for_one, 100, 5}, Supervisors}}.
