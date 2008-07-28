erlang\_couchdb is a really simple CouchDB client. It uses inets and the rfc4627 to create, dispatch, encode and parse requests to and from a CouchDB server. It also uses yaws:url\_encode/1.

I developed this module because the existing modules seemed too big for my taste. This module implements a very simple gen\_server model to let you create a process representing a CouchDB connection that actions can be sent to.

The implemented functionality is really limited because I'm only really implementing the stuff that I'm using in I Play WoW.

* Get server information
* Create database
* Get database information
* Create document
* Create document with specific ID
* Update document
* Get document
* Get all database documents (via get document where id is "\_all\_docs")
* Create design document
* Execute view

A quick demo:

    erlang_couchdb:start(local, "127.0.0.1", "5984").
    erlang_couchdb:call(local, {server_info}).
    erlang_couchdb:call(local, {create_database, "ngerakines"}).
    erlang_couchdb:call(local, {database_info, "ngerakines"}).
    erlang_couchdb:call(local, {fetch_document, "ngerakines", "aabbccddeeffgg112233445566"}).
    erlang_couchdb:call(local, {create_document, "ngerakines", [{"type", <<"spouse">>}, {"name", <<"Carolyn">>}]}).
    erlang_couchdb:call(local, {fetch_document, "ngerakines", "_all_docs"}).
    [_, {"id", VavId}, _] = erlang_couchdb:call(local, {create_document, "ngerakines", [{"type", <<"child">>}, {"name", <<"Vanessa">>}, {"age", 1.9}]}).
    FamilyDesign = {obj, [
        {"_id", <<"_design/family">>},
        {"language", <<"javascript">>},
        {"views", {obj, [
            {"all", {obj, [ {"map", <<"function(doc) { if (doc.type == 'child') { emit(null, doc); } else if (doc.type == 'spouse') { emit(null, doc); } }">>} ]}},
            {"children", {obj, [ {"map", <<"function(doc) { if (doc.type == 'child')  emit(doc.name, doc) }">>} ]}}
        ]}}
    ]}.
    erlang_couchdb:call(local, {create_document, "ngerakines", "_design/family", FamilyDesign}).
    erlang_couchdb:call(local, {view_document, "ngerakines", "family", "all", []}).
    erlang_couchdb:call(local, {view_document, "ngerakines", "family", "children", []}).
    erlang_couchdb:call(local, {view_document, "ngerakines", "family", "children", [{"key", "\"Vanessa\""}]}).
    erlang_couchdb:call(local, {view_document, "ngerakines", "family", "children", [{"key", "\"Unknown\""}]}).
    erlang_couchdb:call(local, {update_document, "ngerakines", binary_to_list(VavId), [{"age", 2}]}).

TODO list:

* Support updating design documents.
* Add better documentation.
* Add test code.
* Determine if the rfc4627 module is available.
* Determine if the yaws\_api is available.

Patches are welcome. For the time being this module should be considered alpha. Support is limited but feel free to contact me via email and submit patches. If you use this module please let me know.
