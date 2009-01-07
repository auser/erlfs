%%%-------------------------------------------------------------------
%%% @private
%%% 
%%% @author Matt Williamson <mwilliamson@dawsdesign.com>
%%% 
%%% @doc This is the top supervisor for the ErlFS client.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlfs_client_sup).

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
init(StartArgs) ->
    ErlFSClient = {erlfs_client_svr,{erlfs_client_svr, start_link, StartArgs},
	      permanent, 2000, worker, [erlfs_client_svr]},
    {ok,{{one_for_one, 0, 1}, [ErlFSClient]}}.

%%====================================================================
%% Internal functions
%%====================================================================
