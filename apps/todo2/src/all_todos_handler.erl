-module(all_todos_handler).


-export([init/3, rest_init/2,
         is_authorized/2, content_types_provided/2, allowed_methods/2, get_todos/2
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

allowed_methods(Req, State) ->
  {[<<"OPTIONS">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, get_todos}
  ], Req, State}.

get_todos(Req, State) ->
  Conn = database:get_connection(),
  UserId = proplists:get_value(user_id, State),

  {ok, _, Todos} = todo_model:get(Conn, UserId, all),

  FRows = [{[{id, binary_to_integer(Id)}, {title, TodoTitle}, {body, TodoBody}, {user_id, UserId}]} || {Id, TodoTitle, TodoBody, _} <- Todos],
  Data = {[{todos, FRows}]},
  JsonBody = jiffy:encode(Data),

  {JsonBody, Req, State}.
