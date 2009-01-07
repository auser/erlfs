%%%-------------------------------------------------------------------
%%% @author Matt Williamson <mwilliamson@dawsdesign.com>
%%%
%%% @doc ErlFS client application. Clients call functions in this 
%%% module to store and retrieve files.
%%%
%%% @headerfile "../include/erlfs.hrl"
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlfs_client).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([store_file/1, get_file/1]).

%%====================================================================
%% API
%%====================================================================
%% @spec store_file(File) -> file_id() | {error, Reason}
%% 
%% @doc Store a file chunk on a specified node. 
%% Used by {@link erlfs_client}.
%% The caller must recover if the specified node is down.
%%
%% @end
%%--------------------------------------------------------------------
store_file(File) ->
    ok.

%%--------------------------------------------------------------------
%% @spec get_file(FileID) -> ok | {error, Reason}
%%     FileID = file_id()
%% 
%% @doc Retrieve a file chunk from a specified node. 
%% Used by {@link erlfs_client}.
%% The caller must try a different node if the specified node is down.
%%
%% @end
%%--------------------------------------------------------------------
get_file(FileID) ->
    ok.

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @private
%%
%% @spec start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% 
%% @doc ** Application Callback ** 
%%
%% This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% 
%% @end
%%--------------------------------------------------------------------
start(_Type, StartArgs) ->
    case erlfs_client_sup:start_link(StartArgs) of
	{ok, Pid} -> 
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @private
%%
%% @spec stop(State) -> void()
%% 
%% @doc ** Application Callback ** 
%%
%% This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%% 
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
