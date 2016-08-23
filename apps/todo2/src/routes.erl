-module(routes).

-export([dispatch/0]).

dispatch() ->
  {'_', [
    to(user_registration),
    to(user_login),
    to(create_todo),
    to(get_todo),
    to(update_todo),
    to(delete_todo)
  ]}.

to(user_registration) ->
  {"/users/register", user_registration_handler, []};

to(user_login) ->
  {"/users/login", user_login_handler, []};

to(get_todo) ->
  {"/todo/:todo_id", get_todo_handler, []};

to(create_todo) ->
  {"/todo/create", create_todo_handler, []};

to(update_todo) ->
  {"/todo/update/:todo_id", update_todo_handler, []};

to(delete_todo) ->
  {"/todo/delete/:todo_id", delete_todo_handler, []}.
