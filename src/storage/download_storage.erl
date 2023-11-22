-module(download_storage).
-author("Zaryn Technologies").
-include("../records.hrl").
-export([upload_file/1, download_file/1, get_file_by_id/1]).

% Upload File 
upload_file(File) ->
    Fun = fun() ->
        ID = nanoid:gen(),
        Download = #download{
            id = ID,
            file = File},
        mnesia:write(Download),
        ID 
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

% Download File using FileID
download_file(FileID) ->
    Fun = fun() ->
        case mnesia:read({download, FileID}) of
            [Download] ->
                Download#download.file;
            [] ->
                {error, not_found}
        end
    end,
    {atomic, Res} = mnesia:transaction(Fun),
    Res.

% Get the File Info using FileID
get_file_by_id(ID) ->
    Res = mnesia:transaction(
          fun() ->
              mnesia:match_object(#download{id = ID, _= '_'})
          end),
    case Res of
        {atomic, []} -> file_not_exist;
        {atomic, [Download]} -> Download;
        _ -> error
    end.