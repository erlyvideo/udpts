-define(D(X), io:format("~p:~p ~p~n",[?MODULE, ?LINE, X])).

-record(client, {
  pid,
  ip,
  name
}).

-record(stream, {
  pid,
  name,
  clients_count = 0,
  memory,
  messages
}).
