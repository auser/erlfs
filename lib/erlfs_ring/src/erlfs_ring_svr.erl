-module (erlfs_ring_svr).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-import (erlang, [monitor/2, demonitor/1]).

-define (SPACE_SIZE, 4).
-define (MONITORING_TIME, 10).

-record (peer, {name,
								predecessor,
								successors = erlang:make_tuple(?SPACE, undefined),
								branches = erlang:make_tuple(?SPACE, undefined),
								data = []}).

-record(state, {peers=[]}).

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
start_link(Name, By) ->
	gen_server:start_link({local, Name}, ?MODULE, [By], [])
start_link(Name) -> 
	gen_server:start_link({local, Name}, ?MODULE, [], []);
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
	% Add monitoring processes that fire on timers here
	start_timed_monitoring_processes(),
  {ok, #state{ 
				peers = []
	}}.

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
% Start a process that is started by...
handle_call({start, Name}, From, State) ->
	register(Name, spawn(?MODULE, init, [Name, From])),
	receive
		{started, Before, After} -> Reply = {ok, Name, Before, After};
		_ -> Reply = error
	end,
	{reply, Reply, State};

handle_call({stop, Name}, _From, State) ->
	Pid = whereis(Name),
	case Pid of
		undefined ->
			ok;
		_ ->
			exit(Pid, kill),
			unregister(Name)	
	end;

handle_call({get_neighborhood, RequestingNode}, _From, State) ->
	{Name, Predecessor, Successors, Successor} = disect_state(State),
	case Name == Successor of
		true ->
			NewSuccessor = RequestingNode,
			monitor(process, NewSuccessor),
			RequestingNode ! {respond_neighbors, RequestingNode, Name, Name},
			NewState = State#peer{successors = setelement(1, State#peer.successors, NewSuccessor),
														branches = setelement(1, State#peer.branches, NewSuccessor)},
			{reply, ok, NewState};
		false ->
			
	end

handle_call({adjust, Neighbor}, _From, State) ->
	Neighbor ! {adjust, self()},
	receive	
		{Name, Before} -> Reply = {Name, Before};
		_ -> Reply = ok
	end,
	{reply, Reply, State};

handle_call({update_branches, Neighbor}, _From, State) ->
	Neighbor ! {update_branches, self()},
	receive
		{Name, State, Branch} -> Reply = {Name, Start, Branch}
	after 10
		Reply = false
	end,
	{reply, Reply, State};

handle_call(Request, _From, State) ->
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
start_timed_monitoring_processes() ->
	{ok}.

disect_state(State) ->
	Name = State#peer.name,
	Predecessor = State#peer.predecessor,
	Successors = State#peer.successors,
	Successor = element(1, Successors),
	{Name, Predecessor, Successors, Successor}.