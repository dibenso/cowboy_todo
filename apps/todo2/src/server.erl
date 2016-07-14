-module(server).

-export([initialize/0]).

initialize() ->
  database_sup:start_link([]),

  db_conn ! {self(), conn},

  receive
    {conn, Conn} ->
      io:format("~w~n", [epgsql:squery(Conn, "SELECT * FROM users;")])
  end,

  Dispatch = routes:dispatch(),

  {ok, _} = cowboy:start_http(my_http_listener, 100, [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
  ).
