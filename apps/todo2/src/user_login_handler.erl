-module(user_login_handler).

-export([init/3, rest_init/2,
         is_authorized/2, content_types_provided/2, allowed_methods/2,
         malformed_request/2, resource_exists/2, content_types_accepted/2,
         login_user/2
        ]).

init(_Transport, Req, Opts) ->
  {upgrade, protocol, cowboy_rest, Req, Opts}.

rest_init(Req, Opts) ->
  {ok, Req, Opts}.

is_authorized(Req, State) ->
  {true, Req, State}.

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
        {ok, UserData} ->
          Email = proplists:get_value(<<"email">>, UserData),
          Password = proplists:get_value(<<"password">>, UserData),

          Bad = lists:any(fun(X) -> X =:= undefined end, [Email, Password]),

          case Bad of
            true ->
              {true, Req2, State};
            false ->
              {false, Req2, [{user, {Email, Password}}|State]}
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
    {<<"application/json">>, login_user}
  ], Req, State}.

login_user(Req, State) ->
  User = proplists:get_value(user, State),
  Conn = database:get_connection(),

  case user_model:login(User) of
    {ok, logged_in} ->
      ok;
    {error, Reason} ->
      error
  end.
