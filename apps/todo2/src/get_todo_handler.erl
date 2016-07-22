-module(get_todo_handler).

-export([init/3, rest_init/2,
         is_authorized/2, content_types_provided/2, allowed_methods/2,
         malformed_request/2, resource_exists/2, get_todo/2
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

malformed_request(Req, State) ->
    {BindingTodoId, Req2} = cowboy_req:binding(todo_id, Req),
    TodoId = binary_to_integer(BindingTodoId),

    Bad = lists:any(fun(X) -> X =:= undefined end, [TodoId]),

    case Bad of
      true ->
        {true, Req2, State};
      false ->
        case is_integer(TodoId) of
          true  -> {false, Req2, [{todo_id, TodoId}|State]};
          false -> {true, Req2, State}
        end
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

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, get_todo}
  ], Req, State}.

get_todo(Req, State) ->
  Conn = database:get_connection(),
  UserId = proplists:get_value(user_id, State),
  TodoId = proplists:get_value(todo_id, State),

  {ok, Todo} = todo_model:get(Conn, UserId, TodoId),

  {_UserId, _TodoId, TodoTitle, TodoBody} = Todo,

  Body = {[{status, <<"ok">>}, {data, {[{todo_id, TodoId}, {user_id, UserId}, {title, TodoTitle}, {body, TodoBody}]}}]},
  JsonBody = jiffy:encode(Body),

  {JsonBody, Req, State}.
