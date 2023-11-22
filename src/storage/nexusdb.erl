-module(nexusdb).
-author("Zaryn Technologies").
-include("../records.hrl").
-export([insert/1, get_nodes/0, get_node_by_id/1, get_node_by_cookie/1]).

insert(Username) ->
    Fun = fun() ->
        ID = nanoid:gen(),
        Node = node(),
        Cookie = erlang:get_cookie(),
        Nexus = #nexus{
            id = ID,
            username = Username,
            node = Node,
            cookie = Cookie,
            chat = []},
        mnesia:write(Nexus)
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

%
get_nodes() ->
    Fun = fun() ->
            mnesia:all_keys(nexus)
          end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

get_node_by_id(ID) ->
    Res = mnesia:transaction(
          fun() ->
              mnesia:match_object(#nexus{id = ID, _= '_'})
          end),
    case Res of
        {atomic, []} -> nexus_not_exist;
        {atomic, [Nexus]} -> Nexus;
        _ -> error
    end.

get_node_by_cookie(Cookie) ->
    Res = mnesia:transaction(
          fun() ->
              mnesia:match_object(#nexus{cookie = Cookie, _= '_'})
          end),
    case Res of
        {atomic, []} -> user_not_exist;
        {atomic, [Nexus]} -> Nexus;
        _ -> error
    end.