%%%-------------------------------------------------------------------
%% @doc todo2 public API
%% @end
%%%-------------------------------------------------------------------

-module(todo2_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    server:initialize(),
    
    todo2_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
