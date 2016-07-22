-module(todo_model).

-export([create/3, exists/3, get/3]).

%% Don't forget to validate lengths and such
create(Conn, {TodoTitle, TodoBody}, UserId) ->
  StrTitle = binary_to_list(TodoTitle),
  StrBody = binary_to_list(TodoBody),

  Query = io_lib:format("INSERT INTO todos (title, body, user_id) VALUES ('~s', '~s', '~B');", [StrTitle, StrBody, UserId]),
  epgsql:squery(Conn, Query),

  {ok, created}.

exists(Conn, UserId, TodoId) ->
  Query = io_lib:format("SELECT COUNT(*) FROM todos WHERE user_id=~B AND id=~B;", [UserId, TodoId]),
  {ok, _, [{Count}]} = epgsql:squery(Conn, Query),

  binary_to_integer(Count) > 0 .

get(Conn, UserId, TodoId) ->
  Query = io_lib:format("SELECT * FROM todos WHERE user_id=~B AND id=~B;", [UserId, TodoId]),
  {ok, _, [{_TodoId, Title, Body, _UserId}]} = epgsql:squery(Conn, Query),

  {ok, {UserId, TodoId, Title, Body}}.
