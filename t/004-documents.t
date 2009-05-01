#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(26),
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
        etap:any(list_to_binary(Database), Databases, "tmp database listed"),
        ok
    end)(),
    
    %% Create a document
    (fun() ->
        etap:fun_is(
            fun ({json,{struct,[{<<"ok">>,true}, {<<"id">>,<<"FooDocument">>}, {<<"rev">>, FooRev}]}}) ->
                    put(foo_rev, FooRev),
                    true;
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
    
    %% Create a document
    (fun() ->
        etap:fun_is(
            fun ({json,{struct,[{<<"ok">>,true}, {<<"id">>, DocID}, {<<"rev">>, DocRev}]}}) ->
                    put(doc_id_1, DocID),
                    put(doc_rev_1, DocRev),
                    true;
                (_) ->
                    false
            end,
            erlang_couchdb:create_document({"localhost", 5984}, Database, [{<<"bar">>, <<"baz">>}]),
            "Creating document"
        ),
        ok
    end)(),

    (fun() ->
        etap:fun_is(
            fun ({json, {struct, Keys}}) ->
                    etap:is(proplists:get_value(<<"_id">>, Keys), get(doc_id_1), "_id ok"),
                    etap:is(proplists:get_value(<<"bar">>, Keys), <<"baz">>, "bar ok"),
                    true;
                (_) -> false
            end,
            erlang_couchdb:retrieve_document({"localhost", 5984}, Database, binary_to_list(get(doc_id_1))),
            "Fetching document"
        ),
        ok
    end)(),
    
    %% Create a document
    (fun() ->
        etap:fun_is(
            fun ({json,{struct,[{<<"ok">>,true}, {<<"id">>, DocID}, {<<"rev">>, _}]}}) ->
                    true;
                (_) ->
                    false
            end,
            erlang_couchdb:create_document({"localhost", 5984}, Database, {struct, [{<<"bar">>, <<"baz">>}]}),
            "Creating document"
        ),
        ok
    end)(),
    
    %% Create a document
    (fun() ->
        Documents = [
            [{<<"username">>, <<"Nick">>}],
            [{<<"username">>, <<"Tom">>}],
            [{<<"username">>, <<"Jan">>}]
        ],
        etap:fun_is(
            fun ({json, [{struct,[{<<"id">>, ID1}, {<<"rev">>, _}]}, {struct,[{<<"id">>, ID2}, {<<"rev">>, _}]}, {struct,[{<<"id">>, ID3}, {<<"rev">>, _}]}]}) -> 
                    put(doc_id_2, ID1),
                    put(doc_id_3, ID2),
                    put(doc_id_4, ID3),                    
                    true;
                (_) ->
                    false
            end,
            erlang_couchdb:create_documents({"localhost", 5984}, Database, Documents),
            "Creating documents"
        ),
        ok
    end)(),
    
    %% Create a document
    (fun() ->
        etap:fun_is(
            fun ({json,{struct,[{<<"ok">>,true}, {<<"id">>, DocID}, {<<"rev">>, _}]}}) ->
                    put(doc_id_5, DocID),
                    true;
                (Other) ->
                    io:format("Other ~p~n", [Other]),
                    false
            end,
            erlang_couchdb:create_attachment(
                {"localhost", 5984},
                Database,
                "DocWithAttachment",
                "t/001-load.t",
                "text/plain"
            ),
            "Creating document attachment"
        ),
        ok
    end)(),
    
    (fun() ->
        RevTuple = {ok, get(doc_id_1), get(doc_rev_1)},
        etap:is(
            RevTuple,
            erlang_couchdb:document_revision({"localhost", 5984}, Database, binary_to_list(get(doc_id_1))),
            "Fetching rev"
        ),
        etap:is(
            RevTuple,
            erlang_couchdb:document_revision({"localhost", 5984}, Database, get(doc_id_1)),
            "Fetching rev"
        ),
        etap:is(
            {ok, undefined, undefined},
            erlang_couchdb:document_revision({"localhost", 5984}, Database, "NotARealDoc"),
            "Fetching rev"
        ),
        etap:fun_is(
            fun ({error, _}) -> true; (Other) -> io:format("Other ~p~n", [Other]), false end,
            erlang_couchdb:document_revision({"google.com", 80}, Database, "NotARealDoc"),
            "Fetching rev"
        ),
        ok
    end)(),

    (fun() ->
        DocID = get(doc_id_1),
        etap:fun_is(
            fun ({json,{struct,[{<<"ok">>,true}, {<<"id">>, DocID}, {<<"rev">>, NewRev}]}}) ->
                    put(doc_rev_1, NewRev),
                    true;
                (_) -> false
            end,
            erlang_couchdb:update_document({"localhost", 5984}, Database, binary_to_list(get(doc_id_1)), [{<<"foo">>, <<"biz">>}, {<<"_rev">>, get(doc_rev_1)}]),
            "Creating document"
        ),
        ok
    end)(),
    
    (fun() ->
        DocID = get(doc_id_1),
        etap:fun_is(
            fun ({json, {struct, Keys}}) ->
                    etap:is(proplists:get_value(<<"_id">>, Keys), DocID, "_id ok"),
                    etap:is(proplists:get_value(<<"foo">>, Keys), <<"biz">>, "foo ok"),
                    true;
                (_) -> false
            end,
            erlang_couchdb:retrieve_document({"localhost", 5984}, Database, binary_to_list(get(doc_id_1))),
            "Fetching document"
        ),
        ok
    end)(),

    (fun() ->
        DocID = get(doc_id_1),
        etap:fun_is(
            fun ({json,{struct,[{<<"ok">>,true}, {<<"id">>, DocID}, {<<"rev">>, NewRev}]}}) ->
                    put(doc_rev_1, NewRev),
                    true;
                (_) -> false
            end,
            erlang_couchdb:update_document({"localhost", 5984}, Database, binary_to_list(get(doc_id_1)), {struct, [{<<"foo">>, <<"buz">>}, {<<"_rev">>, get(doc_rev_1)}]}),
            "Creating document"
        ),
        ok
    end)(),
    
    (fun() ->
        etap:fun_is(
            fun ({json,{struct,[{<<"ok">>, true}, {<<"id">>, <<"FooDocument">>}, {<<"rev">>, _}]}}) -> true;
                (_) -> false
            end,
            erlang_couchdb:delete_document({"localhost", 5984}, Database, "FooDocument", binary_to_list(get(foo_rev))),
            "document deleted"
        ),
        ok
    end)(),
    
    (fun() ->
        etap:is(erlang_couchdb:delete_documents({"localhost", 5984}, Database, [get(doc_id_1), get(doc_id_2), get(doc_id_3)]), {json, []}, "documents deleted"),
        ok
    end)(),

    (fun() ->
        etap:is(erlang_couchdb:delete_database({"localhost", 5984}, Database), ok, "tmp database deleted"),
        ok
    end)(),

    ok.
