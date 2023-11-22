-module(nexus_server).
-author("Zaryn Technologies").
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).
-export([start_link/0, insert/1, get_nodes/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

insert(Username) ->
    gen_server:call(?MODULE, {insert, Username}).

get_nodes() ->
    gen_server:call(?MODULE, {get_nodes}).

% GenServer Callbacks
init([]) ->
    State = #state{},
    {ok, State}.

handle_call({insert, Username}, _File, State) ->
    Res = nexusdb:insert(Username),
    {reply, Res, State};

handle_call({get_nodes}, _From, State = #state{}) ->
    Res = nexusdb:get_nodes(),
    {reply, Res, State};

%
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.