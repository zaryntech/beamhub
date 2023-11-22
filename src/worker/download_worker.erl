-module(download_worker).
-author("Zaryn Technologies").
-export([download_file_worker/2, get_file_info/2]).

% Worker Process for Downloading Files
download_file_worker(ManagerPid, FileID) ->
    try
        case download_storage:download_file(FileID) of
            {error, not_found} ->
                ManagerPid ! {download_completed, FileID, {error, not_found}};
            File ->
                ManagerPid ! {download_completed, FileID, File},
                io:format("Download completed for FileID ~p. File: ~p~n", [FileID, File]),
                File
        end
    catch
        _:_ ->
            ManagerPid ! {download_completed, FileID, {error, worker_crash}}
    end.

get_file_info(ManagerPid, FileID) ->
    try 
        case download_storage:get_file_by_id(FileID) of 
            {error, not_found} ->
                ManagerPid ! {download_completed, FileID, {error, not_found}};
            File ->
                ManagerPid ! {download_completed, FileID, File},
                io:format("File info ~p~n", [File]),
                ok 
        end
    catch 
        _:_ ->
            ManagerPid ! {download_completed, FileID, {error, worker_crash}}
    end.