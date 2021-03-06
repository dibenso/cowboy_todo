-module(create_todo_handler).

-export([init/3, rest_init/2,
         is_authorized/2, content_types_provided/2, allowed_methods/2,
         malformed_request/2, resource_exists/2, content_types_accepted/2,
         create_todo/2
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
  {[<<"OPTIONS">>, <<"POST">>], Req, State}.

malformed_request(Req, State) ->
  case cowboy_req:body(Req) of
    {ok, Data, Req2} ->
      case request:from_json(Data) of
        {ok, TodoData} ->
          Title = proplists:get_value(<<"todo_title">>, TodoData),
          Body = proplists:get_value(<<"todo_body">>, TodoData),

          Bad = lists:any(fun(X) -> X =:= undefined end, [Title, Body]),

          case Bad of
            true ->
              {true, Req2, State};
            false ->
              {false, Req2, [{todo, {Title, Body}}|State]}
          end;
        {error, _} ->
          {true, Req, State}
      end;
    {more, _, Req2} ->
      {true, Req2, State};
    {error, _} ->
      {true, Req, State}
  end.

resource_exists(Req, State) ->
  {true, Req, State}.

content_types_accepted(Req, State) ->
  {[
    {<<"application/json">>, create_todo}
  ], Req, State}.

%% The client should run a check to see if the todo was actually created by querying for it.
create_todo(Req, State) ->
  {TodoTitle, TodoBody} = proplists:get_value(todo, State),
  Conn = database:get_connection(),
  UserId = proplists:get_value(user_id, State),

  todo_model:create(Conn, {TodoTitle, TodoBody}, UserId),

  Body = {[{status, <<"ok">>}, {data, {[{todo_title, TodoTitle}, {todo_body, TodoBody}]}}]},
  JsonBody = jiffy:encode(Body),

  Req2 = cowboy_req:set_resp_body(JsonBody, Req),
  {true, Req2, State}.
