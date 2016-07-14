-module(server).

-export([initialize/0]).

initialize() ->
  database_sup:start_link([]),

  Dispatch = cowboy_router:compile([routes:dispatch()]),

  {ok, _} = cowboy:start_http(my_http_listener, 100, [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
  ).
