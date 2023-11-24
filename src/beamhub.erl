-module(beamhub).
-author("Zaryn Technologies").
-export([erlang_node/1, node_alive/0, connect_node/1, init_mnesia/0]).
-include("records.hrl").

% Start an Erlang Node 
erlang_node(Username) ->
    Atom = list_to_atom(Username),
    case net_kernel:start(Atom, #{name_domain => shortnames}) of
        {ok, Pid} ->
            io:format("Erlang node ~p started successfully.~n", [Atom]),
            Pid;
        Error ->
            io:format("Error starting Erlang node ~p: ~p~n", [Atom, Error]),
            Error
    end.

% Check Node Liveness
node_alive() ->
    is_alive().

% Sets up a connection to Node. Returns pong if it is successful, otherwise pang.
connect_node(Node) ->
    case net_adm:ping(Node) of
        pong ->
            io:format("Connected to node ~p~n", [Node]),
            pong;
        pang ->
            io:format("Failed to connect to node ~p~n", [Node]),
            pang
    end.

% Initialize Mnesia DB
init_mnesia() ->
    application:set_env(mnesia, dir, "Mnesia/"),
    case mnesia:create_schema([node()]) of
        ok ->
            io:format("Mnesia schema created successfully.~n"),
            ok;
        {error, Reason} ->
            io:format("Error creating Mnesia schema: ~p~n", [Reason]),
            {error, Reason}
    end,
    case mnesia:start() of
        ok ->
            io:format("Mnesia started successfully.~n"),
            ok;
        {error, _Reason} ->
            io:format("Error starting Mnesia: ~p~n", [_Reason]),
            {error, _Reason}
    end,
    create_mnesia_tables().

% Helper function to create Mnesia tables
create_mnesia_tables() ->
    TableSpecs = [
        {nexus, [{attributes, record_info(fields, nexus)},
                 {disc_copies, [node()]},
                 {type, ordered_set}]},
        {download, [{attributes, record_info(fields, download)},
                    {disc_copies, [node()]},
                    {type, ordered_set}]},
        {chat, [{attributes, record_info(fields, chat)},
                {disc_copies, [node()]},
                {type, ordered_set}]}
    ],
    lists:foreach(fun({TableName, TableOptions}) ->
                        case mnesia:create_table(TableName, TableOptions) of
                            {atomic, ok} ->
                                io:format("Table ~p created successfully.~n", [TableName]);
                            {atomic, {error, Reason}} ->
                                io:format("Error creating table ~p: ~p~n", [TableName, Reason]);
                            Other ->
                                io:format("Unexpected result creating table ~p: ~p~n", [TableName, Other])
                        end
                    end, TableSpecs).