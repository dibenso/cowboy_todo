-module(todo_model).

-export([create/3]).

%% Don't forget to validate lengths and such
create(Conn, {TodoTitle, TodoBody}, UserId) ->
  StrTitle = binary_to_list(TodoTitle),
  StrBody = binary_to_list(TodoBody),

  Query = io_lib:format("INSERT INTO todos (title, body, user_id) VALUES ('~s', '~s', '~B');", [StrTitle, StrBody, UserId]),
  epgsql:squery(Conn, Query),

  {ok, created}.
