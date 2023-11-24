-record(nexus, {
    id,
    username,
    node,
    cookie,
    chat,
    pid
}).

-record(download, {
    id,
    file,
    status,
    progress,
    pid
}).

-record(chat, {
    id, 
    content,
    sender, 
    recipient
}).