%%%====================================================================
%%% Erlfs main include file.
%%%====================================================================

%% Types
%% @type file_id() = string(). Special ID returned when calling 
%% {@link erlfs_client:file_store/2}.
%% @type chunk_number() = integer(). The nth chunk of a file. 0-based.
%% @type chunk_id() = {file_id(), chunk_number()}.

%% Macros
-define('RPC_TIMEOUT', 2000).

%% Mnesia Records
-record(file_meta, {id="", name="", size=0, type="text/plain",
		    created={{0, 1, 1}, {0, 0, 0}}}).
-record(file, {file_meta=#file_meta{}, data}).

-record(chunk_meta, {file_meta=#file_meta{}, number=0, size=0, nodes=[]}).
-record(chunk, {chunk_meta=#chunk_meta{}, data}).

-record(directory, {path, subdirectories=[]}).
