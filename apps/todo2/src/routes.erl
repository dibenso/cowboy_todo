-module(routes).

-export([dispatch/0]).

dispatch() ->
  {'_', [
    to(user_registration),
    to(user_login)
  ]}.

to(user_registration) ->
  {"/users/register", user_registration_handler, []};

to(user_login) ->
  {"/users/login", user_login_handler, []}.
