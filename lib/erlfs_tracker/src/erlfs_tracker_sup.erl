%%%-------------------------------------------------------------------
%%% @private
%%%
%%% @author Matt Williamson <mwilliamson@mwilliamson-ubuntu-vm>
%%%
%%% @doc This is the top supervisor. It will start and
%%% monitor the ErlFS tracker server.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlfs_tracker_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link(StartArgs) -> {ok,Pid} | ignore | {error,Error}
%%
%% @doc Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
start_link(StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, StartArgs).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec init(StartArgs) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%%    StartArgs = [term()]
%%
%% @doc Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
init(StartArgs) ->
    ErlFSTracker = {erlfs_tracker_svr, 
		   {erlfs_tracker_svr, start_link, StartArgs},
		   permanent, 2000, worker, [erlfs_tracker_svr]},
    {ok, {{one_for_one, 5, 1}, [ErlFSTracker]}}.

%%====================================================================
%% Internal functions
%%====================================================================
