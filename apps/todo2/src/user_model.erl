-module(user_model).

-export([exists/3, create/2, login/2]).

exists(Conn, {Username, Email}, Field) when Field =:= username; Field =:= email ->
  case Field of
    username ->
      QueryString  = io_lib:format("SELECT COUNT(*) FROM users WHERE username='~s';", [Username]),
      Res = epgsql:squery(Conn, QueryString),
      {ok, _, [{Count}]} = Res,

      case list_to_integer(binary_to_list(Count)) of
        0 -> false;
        _ -> true
      end;
    email ->
      QueryString  = io_lib:format("SELECT COUNT(*) FROM users WHERE email='~s';", [Email]),
      Res = epgsql:squery(Conn, QueryString),
      {ok, _, [{Count}]} = Res,

      case list_to_integer(binary_to_list(Count)) of
        0 -> false;
        _ -> true
      end
  end.

create(Conn, User={Username, Email, Password}) ->
  case validate(User) of
    {ok, validated} ->
      StrUsername = binary_to_list(Username),
      StrEmail = binary_to_list(Email),

      {ok, Salt} = bcrypt:gen_salt(),
      {ok, PasswordHash} = bcrypt:hashpw(binary_to_list(Password), Salt),

      Query = io_lib:format("INSERT INTO users (username, email, password_hash) VALUES ('~s', '~s', '~s');", [StrUsername, StrEmail, PasswordHash]),
      epgsql:squery(Conn, Query),

      {ok, created};
    Error ->
      Error
  end.

login(Conn, {Email, Password}) ->
  Query = io_lib:format("SELECT * FROM users WHERE email='~s';", [Email]),
  {ok, _, Res} = epgsql:squery(Conn, Query),

  Error = {error, [<<"invalid email or password">>]},

  case length(Res) > 0 of
    true ->
      [{Id, Username, Email, PasswordHash}] = Res,

      case {ok, binary_to_list(PasswordHash)} =:= bcrypt:hashpw(binary_to_list(Password), binary_to_list(PasswordHash)) of
        true ->
          {ok, {binary_to_integer(Id), Username}};
        false ->
          Error
      end;
    false ->
      Error
  end.

validate({Username, Email, Password}) ->
  StrUsername = binary_to_list(Username),
  StrEmail = binary_to_list(Email),
  StrPassword = binary_to_list(Password),

  GoodUsername = length(StrUsername) >= 8 andalso length(StrUsername) =< 32,
  GoodPassword = length(StrPassword) >= 8 andalso length(StrPassword) =< 64,
  GoodEmail = email_address:is_valid(StrEmail),

  Error1 = case GoodUsername of
             true ->
               [];
             false ->
               <<"username must be between 8 and 32 characters long.">>
           end,

  Error2 = case GoodPassword of
             true ->
               [];
             false ->
               <<"password must be between 8 and 32 characters long.">>
             end,

  Error3 = case GoodEmail of
             true ->
               [];
             false ->
               <<"email is not valid.">>
             end,


  Errors = lists:filter(fun(X) -> length(binary_to_list(X)) > 0 end, lists:flatten([Error1|[Error2|[Error3|[]]]])),

  case length(Errors) of
    0 ->
      {ok, validated};
    1 ->
      {error, Errors}
  end.
