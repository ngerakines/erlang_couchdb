#!/usr/bin/env escript
%% -*- erlang -*-

main(_) ->
    etap:plan(1),
    etap_can:loaded_ok(erlang_couchdb, "Module 'erlang_couchdb' loaded"),
    etap:end_tests(),
    ok.
