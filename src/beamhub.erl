-module(beamhub).
-author("Zaryn Technologies").
-export([erlang_node/1, node_alive/0, connect_node/1, init_mnesia/0]).
-include("records.hrl").

% Start an Erlang Node 
erlang_node(Username) ->
    Atom = list_to_atom(Username),
    {ok, Pid} = net_kernel:start(Atom, #{name_domain => shortnames}),
    Pid.

% Check Node Liveness
node_alive() ->
    is_alive().

% Sets up a connection to Node. Returns pong if it is successful, otherwise pang.
connect_node(Node) ->
    net_adm:ping(Node).

% Initialize Mnesia DB
init_mnesia() ->
    application:set_env(mnesia, dir, "Mnesia/"),
    mnesia:create_schema([node()]),
    mnesia:start(),  
    mnesia:create_table(nexus, [{attributes, record_info(fields, nexus)},
                                {disc_copies, [node()]},
                                {type, ordered_set}]).