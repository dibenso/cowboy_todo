-module(delete_todo_handler).

-export([init/3, rest_init/2,
         is_authorized/2, content_types_provided/2, allowed_methods/2,
         malformed_request/2, resource_exists/2, delete_resource/2
        ]).

init(_Transport, Req, Opts) ->
  {upgrade, protocol, cowboy_rest, Req, Opts}.

rest_init(Req, Opts) ->
  {ok, Req, Opts}.

is_authorized(Req, State) ->
  {Jwt, Req2} = cowboy_req:header(<<"authorization">>, Req),

  case Jwt of
    undefined ->
      {false, Req2, State};
    _         ->
      case ejwt:decode(Jwt, request:jwt_key()) of
        error  ->
          {{false, <<"">>}, Req2, State};
        Claims ->
          UserId = proplists:get_value(<<"user_id">>, Claims),
          {true, Req2, [{user_id, UserId}|State]}
      end
  end.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, no_cb}
  ], Req, State}.

allowed_methods(Req, State) ->
  {[<<"OPTIONS">>, <<"DELETE">>], Req, State}.

malformed_request(Req, State) ->
  {BindingTodoId, Req2} = cowboy_req:binding(todo_id, Req),

  try binary_to_integer(BindingTodoId) of
    TodoId ->
      {false, Req2, [{todo_id, TodoId}|State]}
  catch
    _:_    ->
      {true, Req2, State}
  end.

resource_exists(Req, State) ->
  Conn = database:get_connection(),
  UserId = proplists:get_value(user_id, State),
  TodoId = proplists:get_value(todo_id, State),

  case todo_model:exists(Conn, UserId, TodoId) of
    true  ->
      {true, Req, State};
    false ->
      {false, Req, State}
  end.

delete_resource(Req, State) ->
  Conn = database:get_connection(),
  UserId = proplists:get_value(user_id, State),
  TodoId = proplists:get_value(todo_id, State),

  case todo_model:delete(Conn, UserId, TodoId) of
    {ok, _} ->
      Body = {[{status, <<"ok">>}]},
      JsonBody = jiffy:encode(Body),
      Req2 = cowboy_req:set_resp_body(JsonBody, Req),
      {true, Req2, State};
    _       ->
      {false, Req, State}
  end.
