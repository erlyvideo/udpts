-module(udpts_stats).
-author('Max Lapshin <max@maxidoors.ru>').
-include("udpts.hrl").

-export([html/0, fill_stream_info/1]).


html() ->
  {ok, Bin} = file:read_file("index.html"),
  re:replace(Bin, "<!-- PROCESSES -->", processes_html(), [{return, binary}]).

i2b(Num) when is_integer(Num) -> list_to_binary(integer_to_list(Num));
i2b(T) -> list_to_binary(lists:flatten(io_lib:print("~p", [T]))).


fill_stream_info(#stream{pid = Pid} = Stream) ->
  [{memory,Memory},{message_queue_len,Messages}] = erlang:process_info(Pid, [memory,message_queue_len]),
  Stream#stream{memory = Memory, messages = Messages}.

processes_html() ->
  Streams = lists:map(fun fill_stream_info/1, ets:tab2list(udpts_streams)),
  Now = os:timestamp(),
  [begin
    ["<tr>",
    "<td>", Name, "</td>",
    "<td>", Multicast,"</td>"
    "<td>", i2b(Port),"</td>"
    "<td>", i2b(Clients), "</td>",
    "<td>", i2b(timer:now_diff(LastPacketAt, Now) div 1000000), "</td>",
    "<td>",i2b(Memory), "</td><td>",i2b(Messages) ,"</td>",
    "<td><button onclick=\"stopChannel('", Name ,"')\">Stop</button></td>",
    "</tr>"]
  end || #stream{name = Name, multicast = Multicast, port = Port, clients_count = Clients, memory = Memory, messages = Messages, last_packet_at = LastPacketAt} <- Streams].