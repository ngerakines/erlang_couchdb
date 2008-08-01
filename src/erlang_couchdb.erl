<<<<<<< HEAD:src/erlang_couchdb.erl
%% Copyright (c) 2008 Nick Gerakines <nick@gerakines.net>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%% 
%% @todo Create eUnit tests
%% @todo Add better documentation
%% @todo Merge erlang_couchdb:view_url/6 into erlang_couchdb:build_url/4
%% @author Nick Gerakines <nick@gerakines.net>
%% @copyright 2008 Nick Gerakines
%% @version 0.1r1
%% @doc A simple CouchDB client.
%% 
%% This module was created for the purpose of creating a very small and light
%% CouchDB interface. It supports a limited number of API methods and features.
%% 
%% This module was built for use in the I Play WoW Facebook Application and
%% website. Support is limited and features will be added as needed by the
%% developer/website.
-module(erlang_couchdb).
-behaviour(gen_server).

-author("Nick Gerakines <nick@gerakines.net>").
-version("0.1r1").

%% gen_server exports
-export([
    init/1, terminate/2, code_change/3,
    handle_call/3, handle_cast/2, handle_info/2
]).

%% public functions and API
-export([start/3, call/2]).

%% @private
init([Host, Port]) ->
    {ok, {Host, Port}}.

%% @private
handle_call({info}, _From, State) ->
    {reply, State, State};

handle_call({url, Database, Request}, _From, State = {Host, Port}) ->
    Url = build_url(Host, Port, Database, Request),
    {reply, Url, State};

handle_call({server_info}, _From, State = {Host, Port}) ->
    Url = build_url(Host, Port),
    Response = submit_request(get, Url, 0),
    {reply, Response, State};

handle_call({database_info, Database}, _From, State = {Host, Port}) ->
    Url = build_url(Host, Port, Database),
    Response = submit_request(get, Url, 0),
    {reply, Response, State};

handle_call({create_database, Database}, _From, State = {Host, Port}) ->
    Url = build_url(Host, Port, Database),
    Response = submit_request(put, Url, ""),
    {reply, Response, State};

handle_call({fetch_document, Database, DocID}, _From, State = {Host, Port}) ->
    Url = build_url(Host, Port, Database, DocID),
    Response = submit_request(get, Url, []),
    {reply, Response, State};

handle_call({view_document, Database, ViewClass, ViewId, Args}, _From, State = {Host, Port}) ->
    Url = view_url(Host, Port, Database, ViewClass, ViewId, Args),
    Response = submit_request(get, Url, []),
    {reply, Response, State};

handle_call({create_document, Database, DocID, Properties}, _From, State = {Host, Port}) ->
    Url = build_url(Host, Port, Database, DocID),
    JSON = case Properties of 
        List when is_list(List) -> rfc4627:encode({obj, Properties});
        Obj when is_tuple(Obj) ->rfc4627:encode(Properties)
    end,
    Response = submit_request(put, Url, JSON),
    {reply, Response, State};

handle_call({create_document, Database, Properties}, _From, State = {Host, Port}) ->
    Url = build_url(Host, Port, Database),
    JSON = case Properties of 
        List when is_list(List) -> rfc4627:encode({obj, Properties});
        Obj when is_tuple(Obj) ->rfc4627:encode(Properties)
    end,
    Response = submit_request(post, Url, JSON),
    {reply, Response, State};

handle_call({create_view, Database, DesignString, Properties}, _From, State = {Host, Port}) ->
    Url = build_url(Host, Port, Database, DesignString),
    JSON = case Properties of 
        List when is_list(List) -> rfc4627:encode({obj, Properties});
        Obj when is_tuple(Obj) ->rfc4627:encode(Properties)
    end,
    Response = submit_request(put, Url, JSON),
    {reply, Response, State};

handle_call({update_document, Database, DocID, Properties}, _From, State = {Host, Port}) ->
    Url = build_url(Host, Port, Database, DocID),
    OldData = submit_request(get, Url, []),
    NewProperties =  lists:foldl(
        fun({Key, Value}, Acc) ->
            case lists:keymember(Key, 1, Acc) of
                true -> lists:keyreplace(Key, 1, Acc, {Key, Value});
                false -> [{Key, Value} | Acc]
            end
        end,
        OldData,
        Properties
    ),
    Response = submit_request(put, Url, rfc4627:encode({obj, NewProperties})),
    {reply, Response, State};

handle_call(stop, _From, State) -> {stop, normalStop, State};

handle_call(_, _From, State) -> {reply, ok, State}.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
handle_info(_Info, State) -> {noreply, State}.

%% @private
terminate(_Reason, _State) -> ok.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ---
%% Private functions

%% @private
submit_request(get, Url, _) ->
    io:format("get ~p~n", [Url]),
    case http:request(get, {Url, []}, [], []) of
        {ok, {_Status, _Headers, Body}} ->
            case rfc4627:decode(Body) of
                {ok,{obj, Results}, _} -> Results;
                _ -> {error, parse_error}
            end;
        F -> {error, F}
    end;

submit_request(put, Url, Body) ->
    io:format("put ~p~n", [Url]),
    case http:request(put, {Url, [], "application/json", Body} , [], []) of
        {ok, {_Status, _Headers, ResponseBody}} ->
            case rfc4627:decode(ResponseBody) of
                {ok,{obj, Results}, _} -> Results;
                _ -> {error, parse_error}
            end;
        F -> {error, F}
    end;

submit_request(post, Url, Body) ->
    io:format("post ~p~n", [Url]),
    case http:request(post, {Url, [], "application/json", Body} , [], []) of
        {ok, {_Status, _Headers, ResponseBody}} ->
            case rfc4627:decode(ResponseBody) of
                {ok,{obj, Results}, _} -> Results;
                _ -> {error, parse_error}
            end;
        F -> {error, F}
    end;

submit_request(delete, Url, _) ->
    io:format("delete ~p~n", [Url]),
    case http:request(delete, {Url, []}, [], []) of
        {ok, {_Status, _Headers, Body}} ->
            case rfc4627:decode(Body) of
                {ok,{obj, Results}, _} -> Results;
                _ -> {error, parse_error}
            end;
        F -> {error, F}
    end.

%% @private
build_url(Host, Port) ->
    lists:concat(["http://", Host, ":", Port, "/"]).

%% @private
build_url(Host, Port, Database) ->
    lists:concat(["http://", Host, ":", Port, "/", Database, "/"]).

%% @private
build_url(Host, Port, Database, Request) ->
    lists:concat(["http://", Host, ":", Port, "/", Database, "/", Request]).

%% @private
view_url(Host, Port, Database, ViewName, ViewId, Args) ->
    lists:concat(["http://", Host, ":", Port, "/", Database, "/_view/", ViewName, "/", ViewId, build_querystring(Args, [])]).

%% @private
%% @doc Builds a querystring and url encodes key/value pairs.
build_querystring([], Acc) -> Acc;
build_querystring([{Key, Value} | Tail], []) ->
    Acc = lists:concat(["?", Key, "=", yaws_api:url_encode(Value)]),
    build_querystring(Tail, Acc);
build_querystring([{Key, Value} | Tail], Acc) ->
    NewAcc = lists:concat([Acc, "&", Key, "=", yaws_api:url_encode(Value)]),
    build_querystring(Tail, NewAcc).


%% ---
%% Public Functions / API

%% @spec start(Name, Host, Port) -> Result
%% where 
%%       Name = atom()
%%       Host = string()
%%       Port = string()
%%       Result = {ok, pid()} | {error, any()}
%% @doc Start a server to handle CouchDB requests.
start(Name, Host, Port) ->
    inets:start(),
    gen_server:start_link({local, Name}, ?MODULE, [Host, Port], []).

%% @doc Send a request to a couchdb server.
call(Name, Request) ->
    gen_server:call(Name, Request, infinity).

