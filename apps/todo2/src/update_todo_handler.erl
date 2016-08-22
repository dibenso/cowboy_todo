-module(update_todo_handler).

-export([init/3, rest_init/2,
         is_authorized/2, content_types_accepted/2, content_types_provided/2, allowed_methods/2,
         malformed_request/2, resource_exists/2, previously_existed/2, allow_missing_post/2, update_todo/2
        ]).

init(_Transport, Req, Opts) ->
  {upgrade, protocol, cowboy_rest, Req, Opts}.

rest_init(Req, Opts) ->
  {ok, Req, Opts}.

is_authorized(Req, State) ->
  {Jwt, Req2} = cowboy_req:header(<<"authorization">>, Req),

  case Jwt of
    undefined ->
      {{false, <<"Bearer token_type=\"JWT\"">>}, Req2, State};
    _         ->
      case ejwt:decode(Jwt, request:jwt_key()) of
        error  ->
          {{false, <<"Bearer token_type=\"JWT\"">>}, Req2, State};
        Claims ->
          UserId = proplists:get_value(<<"user_id">>, Claims),
          {true, Req2, [{user_id, UserId}|State]}
      end
  end.

allowed_methods(Req, State) ->
  {[<<"OPTIONS">>, <<"POST">>], Req, State}.

malformed_request(Req, State) ->
  {BindingTodoId, Req2} = cowboy_req:binding(todo_id, Req),

  try binary_to_integer(BindingTodoId) of
    TodoId ->
      Bad = lists:any(fun(X) -> X =:= undefined end, [TodoId]),

      case Bad of
        true ->
          {true, Req2, State};
        false ->
          case is_integer(TodoId) of
            false ->
              {true, Req2, State};
            true ->
              case cowboy_req:body(Req2) of
                {more, _, Req3} ->
                  {true, Req3, State};
                {error, _} ->
                  {true, Req2, State};
                {ok, Data, Req3} ->
                  case request:from_json(Data) of
                    {ok, TodoData} ->
                      Field = proplists:get_value(<<"field">>, TodoData),
                      Value = proplists:get_value(<<"value">>, TodoData),

                      Bad2 = lists:any(fun(X) -> X =:= undefined end, [Field, Value]),

                      case Bad2 of
                        true ->
                          {true, Req3, State};
                        false ->
                          B1 = binary_to_list(Field) =:= "title",
                          B2 = binary_to_list(Field) =:= "body",

                          case B1 or B2 of
                            true  ->
                              {false, Req3, [{todo_id, TodoId}, {field, Field}, {value, Value}|State]};
                            false ->
                              {true, Req3, State}
                          end
                      end;
                    {error, _} ->
                      {true, Req3, State}
                  end
              end
          end
      end
  catch
    _:_ ->
      {true, Req2, State}
  end.

content_types_accepted(Req, State) ->
  {[
    {<<"application/json">>, update_todo}
  ], Req, State}.

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

previously_existed(Req, State) ->
  {false, Req, State}.

allow_missing_post(Req, State) ->
  {false, Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, update_todo}
  ], Req, State}.

update_todo(Req, State) ->
  Conn = database:get_connection(),
  UserId = proplists:get_value(user_id, State),
  TodoId = proplists:get_value(todo_id, State),
  TodoField = list_to_atom(binary_to_list(proplists:get_value(field, State))),
  TodoValue = proplists:get_value(value, State),

  case todo_model:update(Conn, UserId, TodoId, TodoField, TodoValue) of
    {ok, _} ->
      {ok, Todo} = todo_model:get(Conn, UserId, TodoId),

      {_UserId, _TodoId, TodoTitle, TodoBody} = Todo,

      Body = {[{status, <<"ok">>}, {data, {[{todo_id, TodoId}, {user_id, UserId}, {title, TodoTitle}, {body, TodoBody}]}}]},
      JsonBody = jiffy:encode(Body),

      Req2 = cowboy_req:set_resp_body(JsonBody, Req),

      {true, Req2, State};
    _ ->
      {false, Req, State}
  end.
