%%%-------------------------------------------------------------------
%%% @private
%%%
%%% @author Matt Williamson <mwilliamson@dawsdesign.com>
%%%
%%% @doc ErlFS storage server. This does the actual file
%%% chunk storage.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlfs_store_svr).

-include_lib("erlfs.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec: start_link() -> {ok,Pid} | ignore | {error,Error}
%%
%% @doc Starts the server.
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
%% @doc Initiates the server
%%
%% @end
%%--------------------------------------------------------------------
init([]) ->
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
%%--------------------------------------------------------------------
handle_call({store_chunk, Chunk}, From, State) 
  when is_record(Chunk, chunk) ->
    spawn_link(fun() -> store_chunk(From, Chunk) end),
    {reply, storing_chunk, State};

handle_call({get_chunk, ChunkID}, From, State) ->
    spawn_link(fun() -> get_chunk(From, ChunkID) end),
    {reply, getting_chunk, State};

handle_call(Request, _From, State) ->
    % @todo Add error logging here
    io:format("Unknown call: ~p~n", [Request]),
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
%% @doc Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
store_chunk({From, _Ref}, Chunk) ->
    ok = erlfs_store_lib:store_chunk(Chunk),
    %Trackers = pg2:get_members(erlfs_tracker_svr),
    %ok = notify_tracker(Trackers, Chunk#chunk.chunk_meta),
    Message = {store_status, ok},
    From ! Message.

get_chunk({From, _Ref}, ChunkID) ->
    Chunk = erlfs_store_lib:get_chunk(ChunkID),
    Message = {get_chunk, ok, Chunk},
    From ! Message.

notify_tracker([Node|Trackers], ChunkMeta) ->
    %% Notify a tracker that this node has stored a chunk
    %% Try until we get a good tracker or we run out
    Message = {stored_chunk, ChunkMeta, node()},
    case gen_server:call({erlfs_tracker_svr, Node}, Message) of
	{ok, stored_chunk} ->
	    ok;
	_ ->
	    notify_tracker(Trackers, ChunkMeta)
    end;
notify_tracker([], _FileChunk) ->
    {error, notrackers}.
