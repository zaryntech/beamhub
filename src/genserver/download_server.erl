-module(download_server).
-author("Zaryn Technologies").
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {}).
-export([start_link/0, upload_file/1, download_file/1, get_file_by_id/1, get_state/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

upload_file(File) ->
    gen_server:call(?MODULE, {upload_file, File}).
    
download_file(FileID) ->
    gen_server:cast(?MODULE, {download_file, FileID}).

get_file_by_id(FileID) ->
    gen_server:cast(?MODULE, {get_file_by_id, FileID}).
    
get_state() ->
    gen_server:call(?MODULE, get_state).

% GenServer Callbacks
init([]) ->
    State = #state{},
    {ok, State}.

handle_call({upload_file, File}, _From, State) ->
    Res = download_storage:upload_file(File),
    {reply, Res, State};
%  
handle_call(get_state, _From, State) ->
    {reply, State, State}.
    
% 
handle_cast({download_file, FileID}, State) ->
    spawn_link(download_worker, download_file_worker, [self(), FileID]),
    {noreply, State};
    
%
handle_cast({get_file_by_id, FileID}, State) ->
    spawn_link(download_worker, get_file_info, [self(), FileID]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


