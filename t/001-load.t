#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(2),
    etap_can:loaded_ok(erlang_couchdb, "Module 'erlang_couchdb' loaded"),
    etap_can:can_ok(erlang_couchdb, server_info),
    etap:end_tests(),
    ok.
