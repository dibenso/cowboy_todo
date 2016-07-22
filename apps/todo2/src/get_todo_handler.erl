-module(get_todo_handler).

-export([init/3, rest_init/2, is_authorized/2]).

init(_Transport, Req, Opts) ->
  {upgrade, protocol, cowboy_rest, Req, Opts}.

rest_init(Req, Opts) ->
  {ok, Req, Opts}.

is_authorized(Req, State) ->
  {Jwt, Req2} = cowboy_req:header(<<"authorization">>, Req),

  io:format("=============> Jwt: ~w~n", [Jwt]),

  case Jwt of
    undefined ->
      {false, Req2, State};
    _         ->
      case ejwt:decode(Jwt, request:jwt_key()) of
        error  ->
          io:format("Not Authorized :(~n"),

          {false, Req2, State};
        Claims ->
          io:format("Authorized :)~n"),

          UserId = proplists:get_value(<<"user_id">>, Claims),
          {true, Req2, [{user_id, UserId}|State]}
      end
  end.
