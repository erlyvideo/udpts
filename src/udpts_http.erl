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
-include_lib("yaws/include/yaws_api.hrl").


-export([out/1]).



out(Req) ->
  Path = string:strip(Req#arg.pathinfo, left, $/),
  ?D({http, Path, self()}),
  case udpts_reader:subscribe(Path, Req#arg.clisock) of
    {ok, Pid} ->
      {streamcontent_from_pid, "video/mpeg2", Pid};
    {error, enoent} ->
      [{status, 404},{content,"text/html", "No such stream: "++Path}]
  end.
      
