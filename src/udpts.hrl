-define(D(X), io:format("~p:~p ~p~n",[?MODULE, ?LINE, X])).

-record(client, {
  pid,
  ip,
  name
}).

-record(stream, {
  pid,
  name,
  multicast,
  port,
  last_packet_at,
  clients_count = 0
}).
