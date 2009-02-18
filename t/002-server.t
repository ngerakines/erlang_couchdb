#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(2),
    {ok, Data} = erlang_couchdb:server_info({"localhost", 5984}),
    etap:is(proplists:get_value(<<"couchdb">>, Data), <<"Welcome">>, "message ok"),
    etap:is(proplists:get_value(<<"version">>, Data), <<"0.9.0a744813">>, "version ok"),
    etap:end_tests(),
    ok.
