-module(database_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link(Args) ->    
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  {ok, {{one_for_one, 5, 60},
  [{tag1,
   {database, connect, []},
   permanent,
   brutal_kill,
   worker,
   []
  }]
  }}.

%%====================================================================
%% Internal functions
%%====================================================================
