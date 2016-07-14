-module(request).

-export([from_json/1]).

from_json(Json) when is_binary(Json) ->
  case jiffy:decode(Json) of
    {Data} ->
      {ok, Data};
    _      ->
      {error, "Unable to process JSON.~n"}
  end;
from_json(Json) when is_list(Json) ->
  from_json(list_to_binary(Json)).
