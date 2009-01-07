%%%-------------------------------------------------------------------
%%% @private
%%%
%%% @author Matt Williamson <mwilliamson@dawsdesign.com>
%%%
%%% @doc This module contains functions for clients to inter-
%%% act with an ErlFS cluster.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlfs_client_lib).

-include("../include/erlfs.hrl").

%% API
-export([put_file/2, get_file/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec get_file(FileID, Nodes) -> File | {error, Reason}
%%     FileID = file_id()
%%     Nodes = [node()]
%%
%% @doc Retrieve a file stored on an ErlFS cluster.
%%
%% @end
%%--------------------------------------------------------------------
get_file(#file_meta{}, _Nodes) ->
    #file{}.

%%--------------------------------------------------------------------
%% @spec put_file(File, Nodes) -> FileID | {error, Reason}
%%
%% @doc Store a file on an ErlFS cluster.
%%
%% @end
%%--------------------------------------------------------------------
put_file(#file{}, _Nodes) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
