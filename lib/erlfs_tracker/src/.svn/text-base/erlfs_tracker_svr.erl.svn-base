%%%-------------------------------------------------------------------
%%% @private
%%% @author Matt Williamson <mwilliamson@dawsdesign.com>
%%%
%%% @doc This is the ErlFS tracker server. It keeps track of 
%%% where file chunks are stored.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlfs_tracker_svr).

-define(SERVER, ?MODULE).

-include("../include/erlfs.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec  start_link() -> {ok,Pid} | ignore | {error,Error}
%%
%% @doc Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%%
%% @doc Initiates the server.
%%
%% @end
%%--------------------------------------------------------------------
init(_StartArgs) ->
    erlang:process_flag(trap_exit, true),
    pg2:create(?SERVER),
    pg2:join(?SERVER, self()),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%%
%% @doc Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({stored_chunk, ChunkMeta, Node}, From, State) ->
    gen_server:reply(From),
    Success = stored_chunk(ChunkMeta, Node),
    Reply = case Success of
	ok -> {ok, stored_chunk};
	_ -> {error, Success}
    end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%%
%% @doc Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%%
%% @doc Handling all non call/cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%%
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    pg2:leave(?SERVER, self()),
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%%
%% @doc Convert process state when code is changed.
%%
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec stored_chunk(ChunkMeta, Node) -> {ok, written} | {error, Reason}
%%
%% @doc A store node calls this to save where a chunk is stored.
%%
%% @end
%%--------------------------------------------------------------------
stored_chunk(ChunkMeta, Node) ->
    %% @todo Implement. Write record to mnesia.
    error.
