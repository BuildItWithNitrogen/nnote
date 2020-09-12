-module(nnote_db_mnesia).
-record(nnote, {
    id = n_utils:create_id(),
    user_id,
    type,
    date,
    event,
    source,
    topic,
    question,
    tags,
    note
}).
