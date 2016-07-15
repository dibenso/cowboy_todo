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
  StrUsername = binary_to_list(Username),
  StrEmail = binary_to_list(Email),
  StrPassword = binary_to_list(Password),

  GoodUsername = length(StrUsername) >= 8 andalso length(StrUsername) =< 32,
  GoodPassword = length(StrPassword) >= 8 andalso length(StrPassword) =< 64,
  GoodEmail = email_address:is_valid(StrEmail),

  case GoodUsername of
    true ->
      case GoodPassword of
        true ->
          case GoodEmail of
            true ->
              {ok, validated};
            false ->
              {error, ["email address is not valid."]}
          end;
        false ->
          {error, ["password must be between 8 and 64 characters long."]}
      end;
    false ->
      {error, ["username must be between 8 and 32 characters long."]}
  end.
