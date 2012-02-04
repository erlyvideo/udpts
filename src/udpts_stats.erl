-module(udpts_stats).
-author('Max Lapshin <max@maxidoors.ru>').
-include("udpts.hrl").

-export([html/0, json/0]).


html() ->
  {ok, Bin} = file:read_file("index.html"),
  Bin.


json() ->
  Now = os:timestamp(),
  Streams1 = [begin
    [{memory, Memory},{message_queue_len, Messages}] = process_info(Pid, [memory, message_queue_len]),
    SortIndex = case inet_parse:address(Multicast) of
      {ok, {I1,I2,I3,I4}} -> (I1 bsl 24 + I3 bsl 16 + I4 bsl 8 + I2) bsl 16 + Port;
      _ -> Port
    end,
    [{name, list_to_binary(Name)}
     ,{sort_index, SortIndex}
     ,{multicast, list_to_binary(Multicast)}
     ,{port, Port}
     ,{clients, Clients}
     ,{delay, timer:now_diff(Now, LastPacketAt) div 1000000}
     ,{memory, Memory}
     ,{message_queue_len, Messages}
     ,{errors_count, Errors}
     ,{packets_count, Packets}
     ,{scrambled, Scrambled}
    ]
  end || #stream{name = Name, multicast = Multicast, port = Port, clients_count = Clients, pid = Pid, 
         last_packet_at = LastPacketAt, errors_count = Errors, scrambled = Scrambled, packets_count = Packets} <- ets:tab2list(udpts_streams)],
  Streams2 = lists:sort(fun(S1, S2) -> proplists:get_value(sort_index, S1) < proplists:get_value(sort_index, S2) end, Streams1),
  
  mochijson2:encode(Streams2).
