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
%% * v2009-01-23 ngerakines
%%   - Importing functionality from etnt_. GitHub merge didn't work.
%%   - Started adding etap tests.
%% * v0.2.3 2008-10-26: ngerakines
%%   - Added ability to delete databases.
%%   - Added function to fetch Document by ID and return it's Document
%%     ID and revision.
%%   - Added experimental function to create design documents based on
%%     a .js file's contents.
%%   - Fixed bug in parse_view/1 when error is returned.
%% * v0.2.2 2008-10-25: ngerakines
%%   - Applied a patch from Pablo Sortino <psortino@novamens.com> that
%%     provides bulk document creation.
%%   - Created accessor function to create a new document with an ID.
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
%% This code is available as Open Source Software under the MIT license.
%% 
%% Updates at http://github.com/ngerakines/erlang_couchdb/
-module(erlang_couchdb).

-author("Nick Gerakines <nick@gerakines.net>").
-version("Version: 0.2.3").

-export([server_info/1]).
-export([create_database/2, database_info/2, retrieve_all_dbs/1, delete_database/2]).
-export([create_document/3, create_document/4, create_documents/3, create_attachment/5]).
-export([retrieve_document/3, retrieve_document/4, document_revision/3]).
-export([update_document/4, delete_document/4, delete_documents/3]).
-export([create_view/5, create_view/6, invoke_view/5, invoke_multikey_view/6, load_view/4]).
-export([raw_request/5, raw_request/6, fetch_ids/2, parse_view/1]).
-export([get_value/2, set_value/2, set_value/3, fold/2, empty/0]).

%% @private
%% Instead of using ibrowse or http:request/4, this module uses
%% gen_tcp:connect/3. Using ibrowse requires more dependancies and inets can
%% create bottlenecks.
raw_request(Type, Server, Port, URI, Body) ->
    {ok, Socket} = gen_tcp:connect(Server, Port, [binary, {active, false}, {packet, 0}]),
    Req = build_request(Type, URI, Body),
    gen_tcp:send(Socket, Req),
    {ok, Resp} = do_recv(Socket, []),
    gen_tcp:close(Socket),
    {ok,_, ResponseBody} = erlang:decode_packet(http, Resp, []),
    decode_json(parse_response(ResponseBody)).

%% @private
%% Added suport to change the ContentType
raw_request(Type, Server, Port, URI, ContentType, Body) ->
    {ok, Socket} = gen_tcp:connect(Server, Port, [binary, {active, false}, {packet, 0}]),
    Req = build_request(Type, URI, ContentType, Body),
    gen_tcp:send(Socket, Req),
    {ok, Resp} = do_recv(Socket, []),
    gen_tcp:close(Socket),
    {ok,_, ResponseBody} = erlang:decode_packet(http, Resp, []),
    decode_json(parse_response(ResponseBody)).

%% @private
do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            do_recv(Sock, [Bs | B]);
        {error, closed} ->
            {ok, erlang:iolist_to_binary(Bs)}
    end.

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
%% Added suport to change the ContentType
build_request(Type, URI, ContentType, Body) ->
    erlang:iolist_to_binary([
        lists:concat([Type, " ", URI, " HTTP/1.0\r\n"
            "Content-Length: ", erlang:iolist_size(Body), "\r\n"
            "Content-Type: ", ContentType, "\r\n\r\n"
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
    lists:concat(["/", Database]).

%% @private
build_uri(Database, Request) ->
    lists:concat(["/", Database, "/", Request]).

%% @private
build_uri(Database, Request, Attributes) ->
    QueryString = build_querystring(Attributes),
    lists:concat(["/", Database, "/", Request, QueryString]).

%% @private
view_uri(Database, ViewName, ViewId, Args) ->
    lists:concat(["/", Database, "/_design/", ViewName, "/_view/", ViewId, build_querystring(Args)]).

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

%% @spec create_database(DBServer::server_address(), Database::string()) ->  ok | {error, Reason::any()}
%%
%% @type server_address() = {Host::string(), ServerPort::integer()}
%%
%% @doc Create a new database.
create_database({Server, ServerPort}, Database) when is_list(Server), is_integer(ServerPort) ->
    Url = build_uri(Database),
    case raw_request("PUT", Server, ServerPort, Url, []) of
        {json, {struct, [{<<"ok">>, true}]}} -> ok;
        Other -> {error, Other}
    end.

%% @spec delete_database(DBServer::server_address(), Database::string()) ->  ok | {error, Reason::any()}
%%
%% @doc Delete a database.
delete_database({Server, ServerPort}, Database) when is_list(Server), is_integer(ServerPort) ->
    Url = build_uri(Database),
    case raw_request("DELETE", Server, ServerPort, Url, []) of
        {json, {struct, [{<<"ok">>, true}]}} -> ok;
        Other -> {error, Other}
    end.

%% @spec database_info(DBServer::server_address(), Database::string()) ->  {ok, Info::any()} | {error, Reason::any()}
%%
%% @doc Get info about a database.
database_info({Server, ServerPort}, Database) when is_list(Server), is_integer(ServerPort) ->
    Url = build_uri(Database),
    case raw_request("GET", Server, ServerPort, Url, []) of
        {json, {struct, Info}} -> {ok, Info};
        Other -> {error, Other}
    end.

%% @spec server_info(DBServer::server_address()) ->  {ok, Welcome::any()} | {other, Other::any()}
%%
%% @doc Get info about a server.
server_info({Server, ServerPort}) when is_list(Server), is_integer(ServerPort) ->
    Url = build_uri(),
    case raw_request("GET", Server, ServerPort, Url, []) of
        {json, {struct, Welcome}} -> {ok, Welcome};
        Other -> {other, Other}
    end.

%% @spec retrieve_all_dbs(DBServer::server_address()) ->  {ok, Database::any()} | {other, Other::any()}
%%
%% @doc Retieve all the databases
retrieve_all_dbs({Server, ServerPort}) when is_list(Server), is_integer(ServerPort) ->
    case raw_request("GET", Server, ServerPort, "/_all_dbs",[]) of
        {json, Database} -> {ok, Database};
        Other -> {error, Other}
    end.

%% @spec create_attachment(DBServer::server_address(), Database::string(), DocumentID::string(), File::string(), ContentType::string()) -> {"ok": true, "id": "document", "rev": Rev::string()}
%%
%% @doc Create a new attachment document.
create_attachment({Server, ServerPort}, Database, DocumentID, File, ContentType) ->
    {ok, Body} = file:read_file(File),
    Url = build_uri(Database, DocumentID ++ "/attachment"),
    erlang_couchdb:raw_request("PUT", Server, ServerPort, Url, ContentType, Body).

%% @spec create_document(DBServer::server_address(), Database::string(), Attributes::any()) ->  {json, Response::any()} | {raw, Other::any()}
%%
%% @doc Create a new document. This function will create a document with a
%% list of attributes and leaves it up to the server to create an id for it.
%% The attributes should be a list of binary key/value tuples.
create_document({Server, ServerPort}, Database, Attributes) when is_list(Server), is_integer(ServerPort), is_list(Attributes) ->
    create_document({Server, ServerPort}, Database, {struct, Attributes});
create_document({Server, ServerPort}, Database, {struct, _} = Obj) when is_list(Server), is_integer(ServerPort) ->
    Url = build_uri(Database),
    JSON = list_to_binary(mochijson2:encode(Obj)),
    raw_request("POST", Server, ServerPort, Url, JSON).

%% @spec create_document(DBServer::server_address(), Database::string(), DocumentID::string(), Attributes::any()) ->  {json, Response::any()} | {raw, Other::any()}
%%
%% @doc Create a new document with a specific document ID. This is just an
%% accessor function to update_document/4 when the intent is to create a 
%% new document.
create_document({Server, ServerPort}, Database, DocumentID, Attributes) when is_list(Server), is_integer(ServerPort) ->
    update_document({Server, ServerPort}, Database, DocumentID, Attributes).

%% @doc Create many documents in bulk.
%% This function created and submitted by Pablo Sortino, applied on 2008-10-25.
%% @todo Create a spec for this.
create_documents({Server, ServerPort}, Database, Documents) when is_list(Server), is_integer(ServerPort) ->
    Url = build_uri(Database, "_bulk_docs"),
    BulkCreate = {struct, [
        {<<"docs">>, [
            {struct, Doc} || Doc <- Documents
        ]}
    ]},
    JSON = list_to_binary(mochijson2:encode(BulkCreate)),
    raw_request("POST", Server, ServerPort, Url, JSON).

%% @doc Return a tuple containing a document id and the document's latest
%% revision.
%% @todo Create a spec for this.
document_revision({Server, ServerPort}, Database, DocID) when is_binary(DocID) ->
    document_revision({Server, ServerPort}, Database, binary_to_list(DocID));
document_revision({Server, ServerPort}, Database, DocID) when is_list(Server), is_integer(ServerPort) ->
    Url = build_uri(Database, DocID, []),
    case raw_request("GET", Server, ServerPort, Url, []) of
        {json, {struct, Props}} ->
            {ok, proplists:get_value(<<"_id">>, Props, undefined), proplists:get_value(<<"_rev">>, Props, undefined)};
        Other -> {error, Other}
    end.

%% @doc Fetches a document by it's id.
%% @todo Create a spec for this.
retrieve_document({Server, ServerPort}, Database, DocID) ->
    retrieve_document({Server, ServerPort}, Database, DocID, []).

%% @doc Fetches a document by it's id and also some attributes. Attributes
%% should be a list of non binary key/value pair tuples.
%% @todo Create a spec for this.
retrieve_document({Server, ServerPort}, Database, DocID, Attributes) when is_list(Server), is_integer(ServerPort) ->
    Url = build_uri(Database, DocID, Attributes),
    raw_request("GET", Server, ServerPort, Url, []).

%% @doc Sets the attributes for a document with an idea. This function is a
%% bit misleading because it can be used to update an existing document
%% or create a new one with a specified id. If this function is used to
%% update a document the attributes list must contain a '_rev' key/value
%% pair tuple.
%% @todo Create a spec for this.
update_document({Server, ServerPort}, Database, DocID, {struct,_} = Obj) when is_list(Server), is_integer(ServerPort) ->
    Url = build_uri(Database, DocID),
    JSON = list_to_binary(mochijson2:encode(Obj)),
    raw_request("PUT", Server, ServerPort, Url, JSON);
update_document({Server, ServerPort}, Database, DocID, Attributes) when is_list(Server), is_integer(ServerPort) ->
    Url = build_uri(Database, DocID),
    JSON = list_to_binary(mochijson2:encode({struct, Attributes})),
    raw_request("PUT", Server, ServerPort, Url, JSON).

%% @doc Deletes a given document by id and revision.
%% @todo Create a spec for this.
delete_document({Server, ServerPort}, Database, DocID, Revision) when is_list(Server), is_integer(ServerPort) ->
    Url = build_uri(Database, DocID, [{"rev", Revision}]),
    raw_request("DELETE", Server, ServerPort, Url, []).

%% @doc Delete a bunch of documents with a _bulk_docs request.
delete_documents({Server, ServerPort}, Database, Documents) when is_list(Server), is_integer(ServerPort) ->
    Url = build_uri(Database, "_bulk_docs"),
    BulkDelete = {struct, [
        {<<"docs">>, [
            {struct, [{<<"_id">>, Id}, {<<"_rev">>, Rev}, {<<"_deleted">>, true}]} || {Id, Rev} <- Documents
        ]}
    ]},
    JSON = list_to_binary(mochijson2:encode(BulkDelete)),
    raw_request("POST", Server, ServerPort, Url, JSON).

%% @doc Creates a design document. See create_view/6 for more.
%% @todo Create a spec for this.
create_view({Server, ServerPort}, Database, ViewClass, Language, Views) ->
    create_view({Server, ServerPort}, Database, ViewClass, Language, Views, []).

%% @doc Creates or updates a design document. The Views parameter should be
%% a list of tuples representing the view's data. When updating an existing
%% view please be sure to include the _rev field in the Attributes
%% parameter.
%% @todo Create a spec for this.
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
%% @todo Create a spec for this.
invoke_view({Server, ServerPort}, Database, ViewClass, ViewId, Attributes) when is_list(Server), is_integer(ServerPort) ->
    Url = view_uri(Database, ViewClass, ViewId, Attributes),
    raw_request("GET", Server, ServerPort, Url, []).

%% @todo Document this.
%% @todo Create a spec for this.
invoke_multikey_view({Server, ServerPort}, Database, ViewClass, ViewId, Keys, Attributes) when is_list(Server), is_integer(ServerPort) ->
    Url = view_uri(Database, ViewClass, ViewId, Attributes),
    JSON = list_to_binary(mochijson2:encode({struct, [{keys, Keys}]})),
    raw_request("POST", Server, ServerPort, Url, JSON).

%% @doc Return a list of document ids for a given view.
%% @todo Create a spec for this.
parse_view({json, {struct, [{<<"error">>, _Code}, {_, _Reason}]}}) ->
    {0, 0, []};
parse_view({json, Structure}) ->
    {struct, Properties} = Structure,
    TotalRows = proplists:get_value(<<"total_rows">>, Properties, 0),
    Offset = proplists:get_value(<<"offset">>, Properties, 0),
    Data = proplists:get_value(<<"rows">>, Properties, []),
    Ids = [begin
        {struct, Bits} = Rec,
        Id = proplists:get_value(<<"id">>, Bits),
        case proplists:get_value(<<"value">>, Bits, []) of
            [] -> Id;
            {struct, RowValues} -> {Id, RowValues};
            _ -> Id
        end
    end || Rec <- Data],
    {TotalRows, Offset, Ids};
parse_view(_Other) -> {0, 0, []}.

%% @doc Fetch a number of UUIDs from a CouchDB server.
%% @todo Create a spec for this.
fetch_ids({Server, ServerPort}, Limit) ->
    Url = build_uri(lists:concat(["_uuids?count=", Limit])),
    raw_request("POST", Server, ServerPort, Url, []).

%% @doc Create a design document based on a file's contents.
%% Warning! This function is experimental.
load_view({Server, ServerPort}, Database, ViewName, File) ->
    {ok, FH2} = file:open(File, [read, binary]),
    {ok, Data2} = file:read(FH2, 9999),
    erlang_couchdb:create_document(
        {Server, ServerPort}, Database,
        "_design/" ++ ViewName,
        [{<<"language">>, <<"javascript">>}, {<<"views">>, mochijson2:decode(Data2)}]
    ).

%% @doc Get the specified (set of) attribute(s)
%% @todo Create a spec for this.
%% @private
get_value(Path, Struct) when is_list(Path) ->
    get_val(Path, Struct);
get_value(Key, Struct) when is_binary(Key) ->
    {struct, L} = Struct,
    proplists:get_value(Key, L).

%% @private
get_val([Key], Struct) ->
    get_value(Key, Struct);
get_val([Key | T], Struct) ->
    case get_value(Key, Struct) of
        List when is_list(List) -> [get_val(T, X) || X <- List];
        NewStruct when is_tuple(NewStruct) -> get_val(T, NewStruct)
    end.

%% @private
%% @doc Set the specified (set of) attribute(s)
set_value(Path, Value, Struct) when is_list(Path) ->
    [H | T] = lists:reverse(Path),
    set_val(T, Struct, {struct, [{H, Value}]});
set_value(Key, Value, Struct) when is_binary(Key) ->
    extend(Struct, {struct, [{Key, Value}]}).

%% @private
set_val([], Struct, Result) ->
    extend(Struct, Result);
set_val([Key | T], Struct, Result) ->
    set_val(T, Struct, {struct, [{Key, Result}]}).

%% @private
%% @doc To be used with the fold function
set_value(Key, Value) when is_binary(Key) ->
    fun(Struct) -> set_value(Key, vals(Value), Struct) end.

%% @private
vals(B) when is_binary(B) -> B;
vals(I) when is_integer(I) -> I;
vals(L) when is_list(L) -> list_to_binary(L);
vals(A) when is_atom(A) -> vals(atom_to_list(A)).

%% @private
%% @doc Apply a list of set-functions on an initial object.
fold([H|T], Struct) -> fold(T, H(Struct));
fold([], Struct) -> Struct.

%% @private
%% @doc Return an empty object
empty() -> {struct, []}.

%% @private
%% @doc Extend a json obj with one or more json obj (add new leaves and modify the existing ones).
extend(S1, []) -> S1;
extend(S1, [S|T]) ->
    NewS = extend(S1, S),
    extend(NewS, T);
extend(S1, S2) ->
    {struct, L1} = S1,
    {struct, L2} = S2,
    ext(L1, L2, []).

%% @private
ext(L1, [], Result) ->
    {struct, lists:append(Result,L1)};
ext(L1, [{K, {struct, ChildL2}} | T], Result) ->
    case proplists:get_value(K, L1) of
        {struct, ChildL1} ->
            NewL1 = proplists:delete(K, L1),
            ext(NewL1, T, [{K, extend({struct, ChildL1}, {struct, ChildL2})} | Result]);
        _ ->
        NewL1 = proplists:delete(K, L1),
        ext(NewL1, T, [{K, {struct, ChildL2}} | Result])
    end;
ext(L1, [{K, V} | T], Result) ->
    NewL1 = proplists:delete(K, L1),
    ext(NewL1, T, [{K,V} | Result]).
