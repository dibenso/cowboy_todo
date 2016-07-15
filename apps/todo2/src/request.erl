-module(request).

-export([from_json/1, safe_html/1, jwt_key/0]).

jwt_key() ->
  <<"Q7{b/X{^Ch>jYr'.Q]g{Q_6R8b\S$e[[">>.

from_json(Json) when is_binary(Json) ->
  case jiffy:decode(Json) of
    {Data} ->
      {ok, Data};
    _      ->
      {error, "Unable to process JSON.~n"}
  end;
from_json(Json) when is_list(Json) ->
  from_json(list_to_binary(Json)).

safe_html(Data) when is_list(Data) ->
  Unsafe = fun(C) ->
    case C of
      $& -> true;
      $< -> true;
      $> -> true;
      $\ -> true;
      $" -> true;
      $' -> true;
      $` -> true;
      $, -> true;
      $! -> true;
      $@ -> true;
      $$ -> true;
      $% -> true;
      $( -> true;
      $) -> true;
      $= -> true;
      $+ -> true;
      ${ -> true;
      $} -> true;
      $[ -> true;
      $] -> true;
      _  -> false
    end
  end,

  case lists:any(Unsafe, Data) of
    true -> false;
    false -> true
  end;
safe_html(Data) when is_binary(Data) ->
  safe_html(binary_to_list(Data)).
