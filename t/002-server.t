#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(4),
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
    etap_exception:dies_ok(fun() -> erlang_couchdb:server_info({"localhost", 5985}) end, "server_info/1 nok"),
    etap:fun_is(fun ({other, _}) -> true; (_) -> false end, erlang_couchdb:server_info({"example.com", 80}), "Triggering server 'other' response"),
    ok.
