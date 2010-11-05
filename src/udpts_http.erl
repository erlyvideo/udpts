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

-export([args/0]).


args() ->
  [{port,8000},{modules,[udpts_http]},{server_name,"localhost"},{server_root,"."},{document_root,"/tmp"}].
  
