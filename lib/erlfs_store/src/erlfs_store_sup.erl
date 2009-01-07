%%%-------------------------------------------------------------------
%%% @private
%%%
%%% @author Matt Williamson <mwilliamson@mwvmubhhlap>
%%%
%%% @doc This is the top supervisor for the ErlFS storage
%%% application.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlfs_store_sup).

-define(SERVER, ?MODULE).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link(StartArgs) -> {ok,Pid} | ignore | {error,Error}
%%     StartArgs = [term()]
%% 
%% @doc Starts the supervisor.
%%
%% @end
%%--------------------------------------------------------------------
start_link(StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, StartArgs).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%%
%% @doc Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ErlFSStore = {erlfs_store, {erlfs_store_svr, start_link, []},
	      permanent, 2000, worker, [erlfs_store_svr]},
    {ok,{{one_for_all, 0, 1}, [ErlFSStore]}}.

%%====================================================================
%% Internal functions
%%====================================================================
