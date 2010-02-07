-define(b2a(V), list_to_atom(yarmo_bin_util:bin_to_list(V))).
-define(b2l(V), yarmo_bin_util:bin_to_list(V)).
-define(l2b(V), list_to_binary(V)).
-define(a2b(V), atom_to_binary(V, utf8)).
-define(l2a(V), list_to_atom(V)).

-define(timestamp(), calendar:datetime_to_gregorian_seconds(erlang:universaltime())).

-define(DATABASE_NAME, "erlymessage").

-define(LOG(Msg, Data), io:format(Msg ++ " ~p~n", Data)).

-record(request, {context_root = queues, method = 'GET', path = [], params, headers, cookies, body, peer}).

-record(destination, {type = "queue", id, rev, name, max_ttl = 1800, reply_time = 60, ack_mode = "auto"}).

-record(message, {destination, id = generated, rev, max_ttl, headers = [], body, created_timestamp, consumed_timestamp, acknowledged_timestamp}).

-record(batch, {destination, id, rev, max_ttl, body, created_timestamp}).

-record(link, {href, rel = ["alternate"], title, anchor, extensions = []}).

-record(content, {type = "text/plain", src, body = <<>>, summary = []}).