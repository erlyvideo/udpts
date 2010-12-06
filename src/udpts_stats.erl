-module(udpts_stats).
-author('Max Lapshin <max@maxidoors.ru>').
-include("udpts.hrl").

-export([html/0, fill_stream_info/1]).


html() ->
  [<<"<html><body>\n",
  "<style type='text/css'>td {border: 1px black solid;}</style>"
  "<table>\n",
  "<thead><tr><th>Stream</th><th>Clients</th><th>Memory</th><th>Messages</th></tr></thead>\n",
  "<tbody>">>,
  processes_html(),
  <<"</tbody></table></body></html>">>].

i2b(Num) when is_integer(Num) -> list_to_binary(integer_to_list(Num));
i2b(T) -> list_to_binary(lists:flatten(io_lib:print("~p", [T]))).


fill_stream_info(#stream{pid = Pid} = Stream) ->
  [{memory,Memory},{message_queue_len,Messages}] = erlang:process_info(Pid, [memory,message_queue_len]),
  Stream#stream{memory = Memory, messages = Messages}.

processes_html() ->
  Streams = lists:map(fun fill_stream_info/1, ets:tab2list(udpts_streams)),
  ?D({"ZZ:", Streams}),
  [begin
    <<"<tr><td>", (list_to_binary(Name))/binary, "</td><td>", (i2b(Clients))/binary, "</td><td>",
    (i2b(Memory))/binary, "</td><td>",(i2b(Messages))/binary ,"</td></tr>">>
  end || #stream{name = Name, clients_count = Clients, memory = Memory, messages = Messages} <- Streams].