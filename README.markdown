erlang\_couchdb is a really simple CouchDB client. Simple means that it does as little as possible and doesn't get in the way. I developed this module because the existing modules seemed too big and did too much for my taste. This module provides several public functions to do things like manipulating databases, documents and views.

The implemented functionality is really limited because I'm only really implementing the stuff that I'm using in I Play WoW.

* Get server information
* Create database
* Get database information
* Create document
* Create document with specific ID
* Update document
* Get document
* Create design document
* Invoke a design document

A quick demo:

    erlang_couchdb:create_database({"localhost", 5984}, "iplaywow",partitioned).
    erlang_couchdb:create_database({"localhost", 5984}, "iplaywow",not_partitioned).
    erlang_couchdb:database_info({"localhost", 5984}, "iplaywow").
    erlang_couchdb:server_info({"localhost", 5984}).
    erlang_couchdb:create_document({"localhost", 5984}, "iplaywow", [{<<"name">>, <<"Korale">>}, {<<"type">>, <<"character">>}]).
    erlang_couchdb:retrieve_document({"localhost", 5984}, "iplaywow", "0980...").
    erlang_couchdb:update_document({"localhost", 5984}, "iplaywow", "0980...", [{<<"_rev">>, <<"3419...">>}, {<<"name">>, <<"Korale">>}, {<<"level">>, <<"70">>}, {<<"type">>}, <<"character">>}]).
    erlang_couchdb:delete_document({"localhost", 5984}, "iplaywow", "1fd0...", "1193...").
    erlang_couchdb:create_view({"localhost", 5984}, "iplaywow", "characters", <<"javascript">>, [{<<"realm">>, <<"function(doc) { if (doc.type == 'character')  emit(doc.realm_full, null) }">>}]).
    erlang_couchdb:invoke_view({"localhost", 5984}, "iplaywow", "characters", "realm", [{"key", "\"Medivh-US\""}]).

Patches are welcome. For the time being this module should be considered alpha. Support is limited but feel free to contact me via email and submit patches. If you use this module please let me know.

To retrieve object you can do:

    {json, Obj} = erlang_couchdb:invoke_view(...),
    erlang_couchdb:get_value(<<"rows">>, Obj),
    erlang_couchdb:get_value([<<"rows">>,<<"value">>], Obj).

To create an object and set a number of attributes:

    erlang_couchdb:fold([erlang_couchdb:set_value(K, V) || {K,V} <- L],
    erlang_couchdb:empty())

# TODO

 * Document attachments 

Your contributions are welcome.
