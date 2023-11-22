-record(nexus, {
    id,
    username,
    node,
    cookie,
    chat 
}).

-record(download, {
    id,
    file,
    status,
    progress,
    pid
}).