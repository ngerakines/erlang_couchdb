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
%% Change Log:
%% * v0.2.1 2008-10-05: ngerakines
%%   - Complete rewrite with reduced module size.
%%   - Moved away from the rfc4627 module and to mochijson2.
%%   - Moved away from urlencode dependancies from yaws to mochiweb.
%%   - Overall the module 'does less'.
%%   - Moved away from the gen_server model.
%% 
%% @author Nick Gerakines <nick@gerakines.net>
%% @copyright 2008 Nick Gerakines
%% @version 0.2.1
%% @doc A simple CouchDB client.
%% 
%% This module was created for the purpose of creating a very small and light
%% CouchDB interface. It supports a limited number of API methods and features.
%% 
%% This module was built for use in the I Play WoW Facebook Application and
%% website. Support is limited and features will be added as needed by the
%% developer/website.
%% 
%% Updates at http://github.com/ngerakines/erlang_couchdb/
%% 
%% @todo Use gen_tcp instead of inets for http requests.
-module(erlang_couchdb).

-author("Nick Gerakines <nick@gerakines.net>").
-version("Version: 0.2.1").

-export([create_database/2, database_info/2, server_info/1, create_document/3]).
-export([retrieve_document/3, retrieve_document/4, update_document/4, delete_document/4]).
-export([create_view/5, create_view/6, invoke_view/5]).
-export([raw_request/5]).

%% @private
%% Instead of using ibrowse or http:request/4, this module uses
%% gen_tcp:connect/3. Using ibrowse requires more dependancies and inets can
%% create bottlenecks.
raw_request(Type, Server, Port, URI, Body) ->
    {ok, Socket} = gen_tcp:connect(Server, Port, [binary, {active, false}, {packet, 0}]),
    Req = build_request(Type, URI, Body),
    gen_tcp:send(Socket, Req),
    {ok, Resp} = gen_tcp:recv(Socket, 0),
    gen_tcp:close(Socket),
    {ok,_, ResponseBody} = erlang:decode_packet(http, Resp, []),
    decode_json(parse_response(ResponseBody)).

%% @private
%% For a given http response, disregard everything up to the first new line
%% which should be the response body. 99.999% of the time we don't care
%% about the response code or headers. Any sort of error will surface as
%% a parse error in mochijson2:decode/1.
parse_response(<<13,10,13,10,Data/binary>>) -> binary_to_list(Data);
parse_response(<<_X:1/binary,Data/binary>>) -> parse_response(Data).

%% @private
%% Build the HTTP 1.0 request for the Type of request, the URI and
%% optionally a body. If there is a body then find it's length and send that
%% as well. The content-type is hard-coded because this client will never
%% send anything other than json.
build_request(Type, URI, []) ->
    list_to_binary(lists:concat([Type, " ", URI, " HTTP/1.0\r\nContent-Type: application/json\r\n\r\n"]));

build_request(Type, URI, Body) ->
    erlang:iolist_to_binary([
        lists:concat([Type, " ", URI, " HTTP/1.0\r\n"
            "Content-Length: ", erlang:iolist_size(Body), "\r\n"
            "Content-Type: application/json\r\n\r\n"
        ]),
        Body
    ]).

%% @private
%% The build_uri/0, /1, /2, /3 and view_uri/4 functions are used to create
%% the URI's mapping to databases, documents and design documents. Some URIs
%% also have query string parameters which are computed here as well.
%% NOTE: Converting the property list representing query string parameters
%% to the actual query string is done by mochiweb_util:urlencode/1. Make
%% sure that module is in the Erlang lib path.
build_uri() ->
    lists:concat(["/"]).

%% @private
build_uri(Database) ->
    lists:concat(["/", Database, "/"]).

%% @private
build_uri(Database, Request) ->
    lists:concat(["/", Database, "/", Request]).

%% @private
build_uri(Database, Request, Attributes) ->
    QueryString = build_querystring(Attributes),
    lists:concat(["/", Database, "/", Request, QueryString]).

%% @private
view_uri(Database, ViewName, ViewId, Args) ->
    lists:concat(["/", Database, "/_view/", ViewName, "/", ViewId, build_querystring(Args)]).

%% @private
build_querystring([]) -> [];
build_querystring(PropList) ->
    lists:concat(["?", mochiweb_util:urlencode(PropList)]).

%% @private
%% Attempt to decode a JSON body into Erlang structures. If parsing fails 
%% then simply return the raw data and let the user deal with it. This is
%% the only place where a try/catch block is used to minimize this module's
%% interaction with the user's environment.
decode_json(Body) ->
    try mochijson2:decode(Body) of
        Response -> {json, Response}
    catch
        _:_ -> {raw, Body}
    end.

%% ---
%% Public Functions / API

%% @doc Create a new database.
create_database({Server, ServerPort}, Database) when is_list(Server), is_integer(ServerPort) ->
    Url = build_uri(Database),
    raw_request("PUT", Server, ServerPort, Url, []).

%% @doc Get info about a database.
database_info({Server, ServerPort}, Database) when is_list(Server), is_integer(ServerPort) ->
    Url = build_uri(Database),
    raw_request("GET", Server, ServerPort, Url, []).

%% @doc Get info about a server.
server_info({Server, ServerPort}) when is_list(Server), is_integer(ServerPort) ->
    Url = build_uri(),
    raw_request("GET", Server, ServerPort, Url, []).

%% @doc Create a new document. This function will create a document with a
%% list of attributes and leaves it up to the server to create an id for it.
%% The attributes should be a list of binary key/value tuples.
create_document({Server, ServerPort}, Database, Attributes) when is_list(Server), is_integer(ServerPort) ->
    Url = build_uri(Database),
    JSON = list_to_binary(mochijson2:encode({struct, Attributes})),
    raw_request("POST", Server, ServerPort, Url, JSON).

%% @doc Fetches a document by it's id.
retrieve_document({Server, ServerPort}, Database, DocID) ->
    retrieve_document({Server, ServerPort}, Database, DocID, []).

%% @doc Fetches a document by it's id and also some attributes. Attributes
%% should be a list of non binary key/value pair tuples.
retrieve_document({Server, ServerPort}, Database, DocID, Attributes) when is_list(Server), is_integer(ServerPort) ->
    Url = build_uri(Database, DocID, Attributes),
    raw_request("GET", Server, ServerPort, Url, []).

%% @doc Sets the attributes for a document with an idea. This function is a
%% bit misleading because it can be used to update an existing document
%% or create a new one with a specified id. If this function is used to
%% update a document the attributes list must contain a '_rev' key/value
%% pair tuple.
update_document({Server, ServerPort}, Database, DocID, Attributes) when is_list(Server), is_integer(ServerPort) ->
    Url = build_uri(Database, DocID),
    JSON = list_to_binary(mochijson2:encode({struct, Attributes})),
    raw_request("PUT", Server, ServerPort, Url, JSON).

%% @doc Deletes a given document by id and revision.
delete_document({Server, ServerPort}, Database, DocID, Revision) when is_list(Server), is_integer(ServerPort) ->
    Url = build_uri(Database, DocID, [{"rev", Revision}]),
    raw_request("DELETE", Server, ServerPort, Url, []).

%% @doc Creates a design document. See create_view/6 for more.
create_view({Server, ServerPort}, Database, ViewClass, Language, Views) ->
    create_view({Server, ServerPort}, Database, ViewClass, Language, Views, []).

%% @doc Creates or updates a design document. The Views parameter should be
%% a list of tuples representing the view's data. When updating an existing
%% view please be sure to include the _rev field in the Attributes
%% parameter.
create_view({Server, ServerPort}, Database, ViewClass, Language, Views, Attributes)  when is_list(Server), is_integer(ServerPort) ->
    Design = [
        {<<"_id">>, list_to_binary("_design/" ++ ViewClass)},
        {<<"language">>, Language},
        {<<"views">>, {struct, [
            begin
                case View of
                    {Name, Map} -> 
                        {Name, {struct, [{<<"map">>, Map}]}};
                    {Name, Map, Reduce} ->
                        {Name, {struct, [{<<"map">>, Map}, {<<"reduce">>, Reduce}]}}
                end
            end || View <- Views
        ]}}
    | Attributes],
    JSON = list_to_binary(mochijson2:encode({struct, Design})),
    Url = build_uri(Database, "_design/" ++ ViewClass),
    raw_request("PUT", Server, ServerPort, Url, JSON).

%% @doc Executes a view with or without some attributes as modifiers.
invoke_view({Server, ServerPort}, Database, ViewClass, ViewId, Attributes) when is_list(Server), is_integer(ServerPort) ->
    Url = view_uri(Database, ViewClass, ViewId, Attributes),
    raw_request("GET", Server, ServerPort, Url, []).
