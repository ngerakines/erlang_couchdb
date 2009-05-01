#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(2),
    case (catch test()) of
        {'EXIT', Err} ->
            io:format("# ~p~n", [Err]),
            etap:bail();
        _ ->
            etap:end_tests()
    end,
    ok.

test() ->
    {ok, Data} = erlang_couchdb:server_info({"localhost", 5984}),
    etap:is(proplists:get_value(<<"couchdb">>, Data), <<"Welcome">>, "message ok"),
    etap:is(proplists:get_value(<<"version">>, Data), <<"0.9.0">>, "version ok"),
    ok.
