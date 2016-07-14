-module(database).

-export([connect/0, connection/1]).

connect() ->
  io:format("Connected to database.~n"),

  Res = epgsql:connect("localhost", "dillonbenson", "", [
    {database, "dillonbenson"},
    {timeout, 4000}
  ]),

  case Res of
    {ok, Conn} ->
      Pid = spawn_link(fun() -> connection(Conn) end),
      Pid2 = spawn_link(fun() -> ping(Conn) end),
      case whereis(db_conn) of
        undefined ->
          register(db_conn, Pid),
          {ok, Pid2};
        _         ->
          {ok, Pid2}
      end;
    _          ->
      {error, "Unable to connect to database.~n"}
  end.

connection(Conn) ->
  receive
    {From, conn} ->
      From ! {conn, Conn}
  end,
  connection(Conn).

ping(Conn) ->
  timer:sleep(100),
  epgsql:squery(Conn, "SELECT 1;"),
  ping(Conn).
