%% Copyright (c) 2009 Dmitrii 'Mamut' Dimandt <dmitrii@dmitriid.com>
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
%%
%% @author Dmitrii 'Mamut' Dimandt <dmitrii@dmitriid.com>
%% @copyright 2009 Dmitrii 'Mamut' Dimandt
%% @version 0.1
%% @doc A simple stupid wrapper for erlang_couchdb. Requires mochijson2
%%
%% This module was created for the purpose of further simplifying access
%% to an already simple CouchDB interface.
%%
%% This code is available as Open Source Software under the MIT license.

-module(couchdb).

-include("./../include/erlang_couchdb.hrl").

-compile(export_all).


create_database(Name) ->
    erlang_couchdb:create_database(?DB_HOST, Name).
create_database(Server, Name) ->
    erlang_couchdb:create_database(Server, Name).

database_info(Name) ->
    erlang_couchdb:database_info(?DB_HOST, Name).
database_info(Server, Name) ->
    erlang_couchdb:database_info(Server, Name).

server_info() ->
    erlang_couchdb:server_info(?DB_HOST).
server_info({_Host, _Port} = Server) ->
    erlang_couchdb:server_info(Server).

create_document({ID, Doc}) ->
    erlang_couchdb:create_document(?DB_HOST, ?DB_DATABASE, ID, Doc);
create_document(Doc) ->
    erlang_couchdb:create_document(?DB_HOST, ?DB_DATABASE, Doc).

create_document(Db, {ID, Doc}) ->
    erlang_couchdb:create_document(?DB_HOST, Db, ID, Doc);
create_document(Db, Doc) ->
    erlang_couchdb:create_document(?DB_HOST, Db, Doc).

create_document(Host, Db, {ID, Doc}) ->
    erlang_couchdb:create_document(Host, Db, ID, Doc);
create_document(Server, Db, Doc) ->
    erlang_couchdb:create_document(Server, Db, Doc).

retrieve_document(ID) ->
    retrieve_document(?DB_HOST, ?DB_DATABASE, ID).
retrieve_document(Db, ID) ->
    retrieve_document(?DB_HOST, Db, ID).
retrieve_document(Server, Db, ID) ->
    erlang_couchdb:retrieve_document(Server, Db, ID).

%% @spec update_document(ID::string(), Doc::proplist()) ->  ok | {error, Reason::any()}
%%
%% @doc Update only several fields in a document. Leave all other fields unmodified
update_document(ID, Doc) ->
    {json, Document} = retrieve_document(ID),
    Rev = get_rev(Document),
    Doc2 = update_doc_fields(Document, Doc),
    replace_document(ID, Rev, Doc2).
update_document(ID, Rev, Doc) ->
    {json, Document} = retrieve_document(ID),
    Doc2 = update_doc_fields(Document, Doc),
    replace_document(ID, Rev, Doc2).
update_document(Db, ID, Rev, Doc) ->
    {json, Document} = retrieve_document(ID),
    Doc2 = update_doc_fields(Document, Doc),
    replace_document(?DB_HOST, Db, ID, Rev, Doc2).
update_document(Server, Db, ID, Rev, Doc) ->
    {json, Document} = retrieve_document(ID),
    Doc2 = update_doc_fields(Document, Doc),
    replace_document(Server, Db, ID, Rev, Doc2).

%% @spec replace_document(ID::string(), Doc::proplist()) ->  ok | {error, Reason::any()}
%%
%% @doc Replace the doc by a new doc (default erlang_couchdb and couchdb behaviour for updates)
replace_document(ID, Doc) ->
    {json, Document} = retrieve_document(ID),
    Rev = get_rev(Document),
    replace_document(ID, Rev, Doc).
    %erlang_couchdb:update_document(, [{<<"_rev">>, list_to_binary(Rev)} | Doc]).
replace_document(ID, Rev, Doc) ->
    erlang_couchdb:update_document(?DB_HOST, ?DB_DATABASE, ID, [{<<"_rev">>, list_to_binary(Rev)} | Doc]).
replace_document(Db, ID, Rev, Doc) ->
    erlang_couchdb:update_document(?DB_HOST, Db, ID, [{<<"_rev">>, list_to_binary(Rev)} | Doc]).
replace_document(Server, Db, ID, Rev, Doc) ->
    erlang_couchdb:update_document(Server, Db, ID, [{<<"_rev">>, list_to_binary(Rev)} | Doc]).

%% @spec replace_document(ID::string()) ->  ok | {error, Reason::any()}
%%
%% @doc Delete a document
delete_document(ID) ->
    {json, Document} = retrieve_document(ID),
    Rev = get_rev(Document),
    erlang_couchdb:delete_document(?DB_HOST, ?DB_DATABASE, ID, Rev).
delete_document(ID, Rev) ->
    erlang_couchdb:delete_document(?DB_HOST, ?DB_DATABASE, ID, Rev).
delete_document(Db, ID, Rev) ->
    erlang_couchdb:delete_document(?DB_HOST, Db, ID, Rev).
delete_document(Server, Db, ID, Rev) ->
    erlang_couchdb:delete_document(Server, Db, ID, Rev).

create_view(DocName, ViewList) when is_list(ViewList) ->
    erlang_couchdb:create_view(?DB_HOST, ?DB_DATABASE, DocName, <<"javascript">>, ViewList).
    
create_view(DocName, ViewName, Data) ->
    erlang_couchdb:create_view(?DB_HOST, ?DB_DATABASE, DocName, <<"javascript">>, [{ViewName, Data}]).
create_view(DocName, Type, ViewName, Data) ->
    erlang_couchdb:create_view(?DB_HOST, ?DB_DATABASE, DocName, Type, [{ViewName, Data}]).
create_view(Db, DocName, Type, ViewName, Data) ->
    erlang_couchdb:create_view(?DB_HOST, Db, DocName, Type, [{ViewName, Data}]).
create_view(Server, Db, DocName, Type, ViewName, Data) ->
    erlang_couchdb:create_view(Server, Db, DocName, Type, [{ViewName, Data}]).

invoke_view(DocName, ViewName) ->
    erlang_couchdb:invoke_view(?DB_HOST, ?DB_DATABASE, DocName, ViewName, []).
%% @spec invoke_view(DocName::string(), ViewName::string(), Keys::proplist()) ->  result | {error, Reason::any()}
%%
%% @doc Invoke a CouchDB view
invoke_view(DocName, ViewName, Keys) ->
    erlang_couchdb:invoke_view(?DB_HOST, ?DB_DATABASE, DocName, ViewName, Keys).
invoke_view(Db, DocName, ViewName, Keys) ->
    erlang_couchdb:invoke_view(?DB_HOST, Db, DocName, ViewName, Keys).
invoke_view(Server, Db, DocName, ViewName, Keys) ->
    erlang_couchdb:invoke_view(Server, Db, DocName, ViewName, Keys).


%% @spec get_value(DocName::document(), Key::binary()) ->  empty_string | {error, Reason::any()}
%%
%% @type document() = json_struct()
%%
%% @doc Invoke a CouchDB view
get_value(Doc, Key) ->
    get_value(Doc, Key, "").
get_value({struct, L}, Key, DefaultValue) ->
    Values = proplists:get_value(<<"value">>, L, []),
    case Values of
        [] ->
            proplists:get_value(Key, L, DefaultValue);
        {struct, ValueList} ->
            proplists:get_value(Key, ValueList, DefaultValue)
    end;
get_value({json, {struct, ValueList}}, Key, DefaultValue) ->
    proplists:get_value(Key, ValueList, DefaultValue);
get_value(_, _Key, DefaultValue) ->
    DefaultValue.
    
get_id(Doc) ->
    get_id(Doc, "").
get_id({struct, L}, DefaultValue) ->
    binary_to_list(proplists:get_value(<<"id">>, L, DefaultValue));
get_id({json, {struct, L}}, DefaultValue) ->
    binary_to_list(proplists:get_value(<<"id">>, L, DefaultValue));
get_id(_, DefaultValue) ->
    DefaultValue.

get_rev(Doc) ->
    get_rev(Doc, "").
get_rev(Doc, DefaultValue) ->
    binary_to_list(get_value(Doc, <<"_rev">>, DefaultValue)).
    
get_revs(ID) ->
    get_revs(?DB_HOST, ?DB_DATABASE, ID).
get_revs(Db, ID) ->
    get_revs(?DB_HOST, Db, ID).
get_revs(Server, Db, ID) ->
    {ServerName, Port} = Server,
    Type = "GET",
    URI = lists:concat(["/", Db, "/", ID, "?", "revs_info=true"]),
    Body = <<>>,
    Rows = erlang_couchdb:raw_request(Type, ServerName, Port, URI, Body),
    case Rows of
        {json, {struct, PropList}} ->
            Revs = proplists:get_value(<<"_revs_info">>, PropList, []),
            case Revs of
                [] ->
                    [];
                RevList ->
                    get_revision_list(RevList)
            end;
        _ ->
            []
    end.

get_revision_list(RevList) ->
    get_revision_list(RevList, []).
    
get_revision_list([{struct, PropList}|T], Acc) ->
    Rev = binary_to_list(proplists:get_value(<<"rev">>, PropList, <<"">>)),
    Status = binary_to_list(proplists:get_value(<<"status">>, PropList, <<"">>)),
    get_revision_list(
        T,
        Acc ++ [{Rev, Status}]
    );
get_revision_list([], Acc) ->
    Acc.


get_total({json, {struct, L}}) ->
    proplists:get_value(<<"total_rows">>, L, 0);
get_total(_Any) ->
    0.

get_offset({json, {struct, L}}) ->
    proplists:get_value(<<"offset">>, L, 0);
get_offset(_Any) ->
    0.

get_rows({json, {struct, L}}) ->
    proplists:get_value(<<"rows">>, L, []);
get_rows(_Any) ->
    [].


%% @private
%% Update document fields wih new values.
%%
%% @see update_document


update_doc_fields({struct, Doc}, NewFields) ->
    update_doc_fields(Doc, NewFields);

update_doc_fields(OldDoc, []) ->
    OldDoc;
update_doc_fields(OldDoc, [{Key, Value}|T]) ->
    case proplists:get_value(Key, OldDoc) of
        undefined ->
            update_doc_fields([{Key, Value}] ++ OldDoc, T);
        _ ->
            NewDoc = proplists:delete(Key, OldDoc),
            update_doc_fields([{Key, Value}] ++ NewDoc, T)
    end.
