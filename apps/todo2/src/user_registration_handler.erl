-module(user_registration_handler).

-export([init/3, rest_init/2, charsets_provided/2,
         is_authorized/2, content_types_provided/2, allowed_methods/2,
         malformed_request/2, resource_exists/2, is_conflict/2, content_types_accepted/2,
         register_user/2]).

init(_Transport, Req, Opts) ->
  {upgrade, protocol, cowboy_rest, Req, Opts}.

rest_init(Req, Opts) ->
  {ok, Req, Opts}.

charsets_provided(Req, State) ->
  {[<<"UTF-8">>], Req, State}.

is_authorized(Req, State) ->
  {true, Req, State}.

%% Use no_cb in place of a ProvideResource since we're not allowing GET and HEAD
content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, no_cb}
  ], Req, State}.

allowed_methods(Req, State) ->
  {[<<"OPTIONS">>, <<"PUT">>], Req, State}.

malformed_request(Req, State) ->
  case cowboy_req:body(Req) of
    {ok, Data, Req2} ->
      case request:from_json(Data) of
        {ok, UserData} ->
          Username = proplists:get_value(<<"username">>, UserData),
          Email = proplists:get_value(<<"email">>, UserData),
          Password = proplists:get_value(<<"password">>, UserData),

          Bad = lists:any(fun(X) -> X =:= undefined end, [Username, Email, Password]),

          case Bad of
            true ->
              {true, Req2, State};
            false ->
              {false, Req2, [{user, {Username, Email, Password}}|State]}
          end;
        {error, _}     ->
          {true, Req, State}
      end;
    {more, _, Req2} ->
      {true, Req2, State};
    {error, _} ->
      {true, Req, State}
  end.

resource_exists(Req, State) ->
  {false, Req, State}.

is_conflict(Req, State) ->
  {Username, Email, _} = proplists:get_value(user, State),
  Conn = database:get_connection(),

  case user_model:exists(Conn, {Username, Email}, username) of
    true ->
      case user_model:exists(Conn, {Username, Email}, email) of
        true ->
          Body = "{\"status\": \"error\", \"errors\": [\"username already taken\", \"email already taken\"]}\n",
          Req2 = cowboy_req:set_resp_body(Body, Req),
          {true, Req2, State};
        false ->
          Body = "{\"status\": \"error\", \"errors\": [\"username already taken\"]}\n",
          Req2 = cowboy_req:set_resp_body(Body, Req),
          {true, Req2, State}
      end;
    false ->
      case user_model:exists(Conn, {Username, Email}, email) of
        true ->
          Body = "{\"status\": \"error\", \"errors\": [\"email already taken\"]}\n",
          Req2 = cowboy_req:set_resp_body(Body, Req),
          {true, Req2, State};
        false ->
          {false, Req, State}
      end
  end.

content_types_accepted(Req, State) ->
  {[
    {<<"application/json">>, register_user}
  ], Req, State}.

register_user(Req, State) ->
  User = proplists:get_value(user, State),
  Conn = database:get_connection(),

  case user_model:create(Conn, User) of
    {ok, created} ->
      Body = {[{status, <<"ok">>}]},
      JsonBody = jiffy:encode(Body),

      Req2 = cowboy_req:set_resp_body(JsonBody, Req),
      {true, Req2, State};
    {error, Reason} ->
      Body = {[{status, <<"error">>}, {errors, Reason}]},
      JsonBody = jiffy:encode(Body),

      Req2 = cowboy_req:set_resp_body(JsonBody, Req),
      {false, Req2, State}
  end.
