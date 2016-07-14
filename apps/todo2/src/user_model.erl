-module(user_model).

-export([exists/3, create/2]).

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

validate({Username, Email, Password}) ->
  {ok, validated}.
