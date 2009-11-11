-module(erlang_couchdb_SUITE).
-compile(export_all).

-include("ct.hrl").

-define(CONNECTION, {"localhost", 5984}).
-define(DBNAME, "t_erlang_couchdb_test").

%%--------------------------------------------------------------------
%% Test server callback functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: suite() -> DefaultData
%% DefaultData: [tuple()]
%% Description: Require variables and set default values for the suite
%%--------------------------------------------------------------------
suite() -> [{timetrap,{minutes,1}}]
	.

all() ->
	[serverinfo, all_databases, 
	 databaselifecycle, documentlifecycle,
	 createview, viewaccess_nullmap, viewaccess_maponly,
	 parseview
	]     %, viewaccess_mapreduce]
	.

init_per_suite(Config) ->
	crypto:start(),
	inets:start(),
	case erlang_couchdb:database_info(?CONNECTION, ?DBNAME) of
		{ok, _Res} ->
			erlang_couchdb:delete_database(?CONNECTION, ?DBNAME)
			;
		_ -> ok
	end,
	Config
	.

end_per_suite(_Config) ->
	case erlang_couchdb:database_info(?CONNECTION, ?DBNAME) of
		{ok, _Res} ->
			erlang_couchdb:delete_database(?CONNECTION, ?DBNAME)
			;
		_ -> ok
	end,
	inets:stop(),
	ok
	.

serverinfo() ->
	[{userdata,[{doc,"Server connection, and information"}]}]
	.

serverinfo(_Config) ->
    {ok, Res} = erlang_couchdb:server_info(?CONNECTION),
    ct:print(test_category, "server info ~p~n", [Res])
	.

all_databases() ->
	[{userdata,[{doc,"all databases information"}]}]
	.

all_databases(_Config) ->
	{ok, Database} = erlang_couchdb:retrieve_all_dbs(?CONNECTION),
    ct:print(test_category, "all databases ~p~n", [Database])
    .

databaselifecycle(_Config) ->
    ok = erlang_couchdb:create_database(?CONNECTION, ?DBNAME),
    {ok, Res} = erlang_couchdb:database_info(?CONNECTION, ?DBNAME),
    ct:print(test_category, "database info for ~p~n~p~n", [?DBNAME, Res]),
    ok = erlang_couchdb:delete_database(?CONNECTION, ?DBNAME)
    .

documentlifecycle() ->
    [{userdata,[{doc,"Document creation, retrieve, and deletion"}]}]
    .

documentlifecycle(_Config) ->
	% setup
    ok = erlang_couchdb:create_database(?CONNECTION, ?DBNAME),

    {json,{struct,[_, {<<"id">>, Id},_]}} = erlang_couchdb:create_document(?CONNECTION, ?DBNAME, {struct, [{<<"foo">>, <<"bar">> } ]}),
    ct:print(test_category, "id: ~p", [Id]),
    Doc = erlang_couchdb:retrieve_document(?CONNECTION, ?DBNAME, binary_to_list((Id))),
    ct:print(test_category, "Document: ~p", [Doc]),

	% tear down
    ok = erlang_couchdb:delete_database(?CONNECTION, ?DBNAME)
    .


createview() ->
    [{userdata,[{doc,"Documents creation, set view, and deletion"}]}]
    .

createview(_Config) ->
    %   setup
    ok = erlang_couchdb:create_database(?CONNECTION, ?DBNAME),
    {json,{struct,[_, {<<"id">>, Id},_]}} = erlang_couchdb:create_document(?CONNECTION, ?DBNAME, {struct, [{<<"type">>, <<"D">> } ]}),
    {json,{struct,[_, {<<"id">>, Id2},_]}} = erlang_couchdb:create_document(?CONNECTION, ?DBNAME, {struct, [{<<"type">>, <<"S">> } ]}),
    Doc = erlang_couchdb:retrieve_document(?CONNECTION, ?DBNAME, binary_to_list((Id))),
    ct:print(test_category, "Document: ~p", [Doc]),
    Doc2 = erlang_couchdb:retrieve_document(?CONNECTION, ?DBNAME, binary_to_list(Id2)),
    ct:print(test_category, "Document2: ~p", [Doc2]),
	View1 = "function(doc) { if(doc.type) { emit(doc.type)}}",
    Views = [{"all", View1}],
    Res = erlang_couchdb:create_view(?CONNECTION, ?DBNAME, "testview", <<"javasccript">>, Views, []),
    ct:print("view creation result: ~p~n",[Res]),

	%   tear down
    ok = erlang_couchdb:delete_database(?CONNECTION, ?DBNAME)
    .

viewaccess_nullmap() ->
	[{userdata,[{doc,"Documents creation, set null selection view, accessview and deletion"}]}]	.

viewaccess_nullmap(_Config) ->
	do_viewaccess_maponly("function(doc) { emit(null, doc)}")
	.

viewaccess_maponly() ->
	[{userdata,[{doc,"Documents creation, set view, accessview and deletion"}]}]	.

viewaccess_maponly(_Config) ->
	do_viewaccess_maponly("function(doc) { if(doc.type) {emit(null, doc.type)}}")
	.

do_viewaccess_maponly(Viewsource) ->
	do_viewaccess_maponly(Viewsource, fun(X) -> X end)
	.

do_viewaccess_maponly(Viewsource, Cb) ->
	%   setup
	ok = erlang_couchdb:create_database(?CONNECTION, ?DBNAME),
	{json,{struct,[_, {<<"id">>, Id},_]}} = erlang_couchdb:create_document(?CONNECTION, ?DBNAME, {struct, [{<<"type">>, <<"D">> } ]}),
	{json,{struct,[_, {<<"id">>, Id2},_]}} = erlang_couchdb:create_document(?CONNECTION, ?DBNAME, {struct, [{<<"type">>, <<"S">> } ]}),
	Doc = erlang_couchdb:retrieve_document(?CONNECTION, ?DBNAME, binary_to_list((Id))),
	ct:print(test_category, "Document: ~p", [Doc]),
	Doc2 = erlang_couchdb:retrieve_document(?CONNECTION, ?DBNAME, binary_to_list(Id2)),
	ct:print(test_category, "Document2: ~p", [Doc2]),
	Views = [{"all", Viewsource}],
	Res = erlang_couchdb:create_view(?CONNECTION, ?DBNAME, "testview", "javascript", Views, []),
	ct:print("view creation result: ~p~n",[Res]),

	% view access
	ResView = Cb(erlang_couchdb:invoke_view(?CONNECTION, ?DBNAME, "testview", "all",[])),
	ct:print("view access result: ~p~n",[ResView]),

	%   tear down
	ok = erlang_couchdb:delete_database(?CONNECTION, ?DBNAME)
	.

parseview() ->
	[{userdata,[{doc,"Parse View request result"}]}]
	.

parseview(_Config) ->
	ct:print("parse_view~n",[]),
	do_viewaccess_maponly("function(doc) { if(doc.type) {emit(null, doc.type)}}", fun(X) -> erlang_couchdb:parse_view(X) end)
	.
