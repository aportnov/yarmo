-define(b2a(V), list_to_atom(binary_to_list(V))).
-define(b2l(V), binary_to_list(V)).
-define(l2b(V), list_to_binary(V)).
-define(a2b(V), atom_to_binary(V, utf8)).
-define(l2a(V), list_to_atom(V)).

-define(timestamp(), calendar:datetime_to_gregorian_seconds(erlang:universaltime())).

-define(DATABASE_NAME, "erlymessage").

-define(DB_READ(Database, Key), couchdb:retrieve_document(Database, Key)).
-define(DB_CREATE_ID(Database, Key, Document), couchdb:create_document(Database, {Key, Document})).
-define(DB_CREATE(Database, Document), couchdb:create_document(Database, Document)).

-define(LOG(Msg, Data), io:format(Msg ++ " ~p~n", Data)).

-record(request, {context_root = queues, method = 'GET', path = [], params, headers, cookies, body, peer}).

-record(destination, {type = queue, id, name, max_ttl = 1800, reply_time = 60}).

-record(message, {destination, id = generated, max_ttl, headers = [], body, created_timestamp}).

-record(batch, {destination, id, max_ttl, created_timestamp}).

-record(link, {href, rel = ["alternate"], title, anchor, extensions = []}).