#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    %% @todo set this plan
    etap:plan(unknown),
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
        ok
    end)(),
    
    (fun() ->
        {ok, Databases} = erlang_couchdb:retrieve_all_dbs({"localhost", 5984}),
        etap:is(Databases, [list_to_binary(Database)], "tmp database listed"),
        ok
    end)(),
    
    %% Create a document
    (fun() ->
        etap:fun_is(
            fun ({json,{struct,[{<<"ok">>,true}, {<<"id">>,<<"FooDocument">>}, {<<"rev">>, _}]}}) -> true;
                (_) -> false
            end,
            erlang_couchdb:create_document({"localhost", 5984}, Database, "FooDocument", [{<<"foo">>, <<"bar">>}]),
            "Creating document"
        ),
        ok
    end)(),
    
    %% Fetch back that document
    (fun() ->
        etap:fun_is(
            fun ({json, {struct, Keys}}) ->
                    etap:is(proplists:get_value(<<"_id">>, Keys), <<"FooDocument">>, "_id ok"),
                    etap:is(proplists:get_value(<<"foo">>, Keys), <<"bar">>, "foo ok"),
                    true;
                (_) -> false
            end,
            erlang_couchdb:retrieve_document({"localhost", 5984}, Database, "FooDocument"),
            "Fetching document"
        ),
        ok
    end)(),
    
    (fun() ->
        etap:is(erlang_couchdb:delete_database({"localhost", 5984}, Database), ok, "tmp database created"),
        ok
    end)(),

    ok.
