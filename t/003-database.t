#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(14),
    pre_run(),
    test(),
    etap:end_tests(),
    ok.

pre_run() ->
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    ok.

database() ->
    lists:flatten([
        [[random:uniform(25) + 96] || _ <-lists:seq(1,5)],
        [[random:uniform(9) + 47] || _ <-lists:seq(1,3)]
    ]).

test() ->
    Database = database(),

    (fun() ->
        etap:is(erlang_couchdb:create_database({"localhost", 5984}, Database), ok, "tmp database created"),
        {ok, DatabaseProps} = erlang_couchdb:database_info({"localhost", 5984}, Database),
        etap:is(proplists:get_value(<<"db_name">>, DatabaseProps), list_to_binary(Database), "name ok"),
        etap:is(proplists:get_value(<<"doc_count">>, DatabaseProps), 0, "document count ok"),
        etap:is(proplists:get_value(<<"doc_del_count">>, DatabaseProps), 0, "document delete count ok"),
        etap:is(proplists:get_value(<<"update_seq">>, DatabaseProps), 0, "update count ok"),
        etap:is(proplists:get_value(<<"purge_seq">>, DatabaseProps), 0, "purge count ok"),
        etap:is(proplists:get_value(<<"compact_running">>, DatabaseProps), false, "compaction status ok"),
        ok
    end)(),
    
    (fun() ->
        {ok, Databases} = erlang_couchdb:retrieve_all_dbs({"localhost", 5984}),
        etap:any(list_to_binary(Database), Databases, "tmp database listed"),
        ok
    end)(),
    
    (fun() ->
        etap:fun_is(fun ({error, _}) -> true; (_) -> false end, erlang_couchdb:retrieve_all_dbs({"example.com", 80}), "Triggering server 'other' response"),
        ok
    end)(),
    
    (fun() ->
        etap:fun_is(fun ({error, _}) -> true; (_) -> false end, erlang_couchdb:database_info({"example.com", 80}, "asdasdasd"), "Triggering server 'other' response"),
        ok
    end)(),

    (fun() ->
        Error = {ok,[{<<"error">>,<<"not_found">>}, {<<"reason">>,<<"Missing">>}]},
        etap:is(erlang_couchdb:database_info({"localhost", 5984}, "hahahahano"), Error, "database_info/2 on non-existing db."),
        ok
    end)(),

    (fun() ->
        Error = {error,{json,{struct,[{<<"error">>,<<"file_exists">>}, {<<"reason">>, <<"The database could not be created, the file already exists.">>}]}}},
        etap:is(erlang_couchdb:create_database({"localhost", 5984}, Database), Error, "tmp database created"),
        ok
    end)(),

    (fun() ->
        etap:is(erlang_couchdb:delete_database({"localhost", 5984}, Database), ok, "tmp database created"),
        ok
    end)(),

    (fun() ->
        Error = {error,{json,{struct,[{<<"error">>,<<"not_found">>}, {<<"reason">>,<<"Missing">>}]}}},
        etap:is(erlang_couchdb:delete_database({"localhost", 5984}, Database), Error, "tmp database created"),
        ok
    end)(),

    ok.
