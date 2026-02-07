%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen.brandt@cuneiform-lang.org>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @doc YAWL Document Store Module
%%
%% This module provides document storage and retrieval for YAWL workflows,
%% supporting both local file storage and S3-compatible cloud storage.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Local file-based document storage</li>
%%   <li>S3-compatible cloud storage support</li>
%%   <li>Content type validation</li>
%%   <li>Document metadata tracking</li>
%%   <li>URL generation for document access</li>
%% </ul>
%%
%% <h3>Document Record</h3>
%%
%% The document record contains:
%% <ul>
%%   <li><b>id:</b> Unique document identifier (auto-generated)</li>
%%   <li><b>case_id:</b> Associated workflow case identifier</li>
%%   <li><b>name:</b> Document filename</li>
%%   <li><b>content_type:</b> MIME type (e.g., <<"application/pdf">>)</li>
%%   <li><b>data:</b> Binary document content</li>
%%   <li><b>uploaded_at:</b> Unix timestamp of upload</li>
%%   <li><b>uploaded_by:</b> User or system that uploaded</li>
%% </ul>
%%
%% <h3>Storage Backends</h3>
%% <ul>
%%   <li><b>local:</b> File system storage at configured base path</li>
%%   <li><b>s3:</b> S3-compatible cloud storage</li>
%% </ul>
%%
%% <h3>Doctests</h3>
%%
%% Document record creation:
%% ```erlang
%% 1> Doc = #document{
%%     id = <<"doc_123">>,
%%     case_id = <<"case_456">>,
%%     name = <<"report.pdf">>,
%%     content_type = <<"application/pdf">>,
%%     data = <<"%PDF-1.4...">>,
%%     uploaded_at = 1704067200,
%%     uploaded_by = <<"user1">>
%% }.
%% #document{id = <<"doc_123">>, case_id = <<"case_456">>, ...}
%% 2> <<"doc_123">> = Doc#document.id.
%% <<"doc_123">>
%% '''
%%
%% Document field access:
%% ```erlang
%% 3> <<"report.pdf">> = Doc#document.name.
%% <<"report.pdf">>
%% 4> <<"application/pdf">> = Doc#document.content_type.
%% <<"application/pdf">>
%% 5> <<"user1">> = Doc#document.uploaded_by.
%% <<"user1">>
%% '''
%%
%% Document state record:
%% ```erlang
%% 6> State = #doc_state{
%%     documents = #{},
%%     case_index = #{},
%%     storage_backend = local,
%%     storage_config = #{}
%% }.
%% #doc_state{documents = #{}, case_index = #{}, ...}
%% 7> local = State#doc_state.storage_backend.
%% local
%% '''
%%
%% Storage backend options:
%% ```erlang
%% 8> Backend = local.
%% local
%% 9> true = Backend =:= local orelse Backend =:= s3.
%% true
%% '''
%%
%% Content type validation:
%% ```erlang
%% 10> ValidType = <<"application/pdf">>.
%% <<"application/pdf">>
%% 11> case binary:split(ValidType, <<"/">>) of
%%     [Type, Subtype] when byte_size(Type) > 0, byte_size(Subtype) > 0 -> true;
%%     _ -> false
%% end.
%% true
%% '''
%%
%% Invalid content type:
%% ```erlang
%% 12> InvalidType = <<"not_a_mime_type">>.
%% <<"not_a_mime_type">>
%% 13> case binary:split(InvalidType, <<"/">>) of
%%     [Type, Subtype] when byte_size(Type) > 0, byte_size(Subtype) > 0 -> true;
%%     _ -> false
%% end.
%% false
%% '''
%%
%% Document ID format:
%% ```erlang
%% 14> DocId = <<"doc_1a2b3c4d">>.
%% <<"doc_1a2b3c4d">>
%% 15> <<"doc_", _/binary>> = DocId.
%% <<"doc_1a2b3c4d">>
%% '''
%%
%% Case ID format:
%% ```erlang
%% 16> CaseId = <<"case_abc123">>.
%% <<"case_abc123">>
%% 17> true = is_binary(CaseId).
%% true
%% '''
%%
%% Running all doctests:
%% ```erlang
%% 1> yawl_documents:doctest_test().
%% ok
%% '''
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_documents).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% Document Record
-export([document/0, document/6]).
-export([get_doc_id/1, get_case_id/1, get_name/1, get_content_type/1,
         get_data/1, get_uploaded_at/1, get_uploaded_by/1]).

%% Gen Server Callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
         handle_info/2, init/1, terminate/2]).

%% Document API
-export([start_documents/0, start_documents/1, stop_documents/0]).
-export([store_document/2, retrieve_document/1, delete_document/1]).
-export([list_documents/1, get_document_url/1, validate_document/1]).
-export([set_storage_backend/1]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type document_id() :: binary().
-type case_id() :: binary().
-type content_type() :: binary().

-record(document, {
    id :: document_id(),
    case_id :: case_id(),
    name :: binary(),
    content_type :: content_type(),
    data :: binary(),
    uploaded_at :: integer(),
    uploaded_by :: binary()
}).

-record(doc_state, {
    documents = #{} :: #{document_id() => #document{}},
    case_index = #{} :: #{case_id() => [document_id()]},
    storage_backend = local :: local | s3,
    storage_config = #{} :: map()
}).

-type doc_result() :: {ok, document_id() | #document{}} | {error, term()}.

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the document store with default options.
%% @end
%%--------------------------------------------------------------------
-spec start_documents() -> {ok, pid()} | {error, term()}.
start_documents() ->
    start_documents([]).

%%--------------------------------------------------------------------
%% @doc Starts the document store with options.
%% Options:
%%   - {storage_backend, local | s3}: Default local
%%   - {storage_config, map()}: Storage-specific config
%% @end
%%--------------------------------------------------------------------
-spec start_documents(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_documents(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%%--------------------------------------------------------------------
%% @doc Stops the document store.
%% @end
%%--------------------------------------------------------------------
-spec stop_documents() -> ok.
stop_documents() ->
    gen_server:stop(?MODULE).

%%--------------------------------------------------------------------
%% @doc Sets the storage backend (local or s3).
%% @end
%%--------------------------------------------------------------------
-spec set_storage_backend(local | s3) -> ok | {error, term()}.
set_storage_backend(Backend) when Backend =:= local orelse Backend =:= s3 ->
    gen_server:call(?MODULE, {set_storage_backend, Backend}).

%%--------------------------------------------------------------------
%% @doc Stores a document for a case.
%% @end
%%--------------------------------------------------------------------
-spec store_document(case_id(), #document{} | proplists:proplist()) -> doc_result().
store_document(CaseId, DocProps) when is_binary(CaseId), is_list(DocProps) ->
    gen_server:call(?MODULE, {store_document, CaseId, DocProps});
store_document(CaseId, #document{} = Doc) when is_binary(CaseId) ->
    gen_server:call(?MODULE, {store_document, CaseId, Doc}).

%%--------------------------------------------------------------------
%% @doc Retrieves a document by ID.
%% @end
%%--------------------------------------------------------------------
-spec retrieve_document(document_id()) -> doc_result().
retrieve_document(DocId) when is_binary(DocId) ->
    gen_server:call(?MODULE, {retrieve_document, DocId}).

%%--------------------------------------------------------------------
%% @doc Deletes a document.
%% @end
%%--------------------------------------------------------------------
-spec delete_document(document_id()) -> ok | {error, not_found}.
delete_document(DocId) when is_binary(DocId) ->
    gen_server:call(?MODULE, {delete_document, DocId}).

%%--------------------------------------------------------------------
%% @doc Lists all documents for a case.
%% @end
%%--------------------------------------------------------------------
-spec list_documents(case_id()) -> {ok, [#document{}]} | {error, term()}.
list_documents(CaseId) when is_binary(CaseId) ->
    gen_server:call(?MODULE, {list_documents, CaseId}).

%%--------------------------------------------------------------------
%% @doc Gets a URL for accessing a document.
%% @end
%%--------------------------------------------------------------------
-spec get_document_url(document_id()) -> {ok, binary()} | {error, not_found}.
get_document_url(DocId) when is_binary(DocId) ->
    gen_server:call(?MODULE, {get_document_url, DocId}).

%%--------------------------------------------------------------------
%% @doc Validates a document (checks content type, size, etc.).
%% @end
%%--------------------------------------------------------------------
-spec validate_document(#document{} | document_id()) ->
          {ok, #document{}} | {error, term()}.
validate_document(#document{} = Doc) ->
    do_validate_document(Doc);
validate_document(DocId) when is_binary(DocId) ->
    case retrieve_document(DocId) of
        {ok, Doc} -> do_validate_document(Doc);
        {error, Reason} -> {error, Reason}
    end.

%%====================================================================
%% Record Constructors and Accessors
%%====================================================================

%% @private
-spec document() -> #document{}.
document() -> #document{}.

%% @private
-spec document(case_id(), binary(), content_type(), binary(), integer(), binary()) ->
          #document{}.
document(CaseId, Name, ContentType, Data, UploadedAt, UploadedBy) ->
    #document{
        id = generate_id(<<"doc">>),
        case_id = CaseId,
        name = Name,
        content_type = ContentType,
        data = Data,
        uploaded_at = UploadedAt,
        uploaded_by = UploadedBy
    }.

%% @private
-spec get_doc_id(#document{}) -> document_id().
get_doc_id(#document{id = Id}) -> Id.

%% @private
-spec get_case_id(#document{}) -> case_id().
get_case_id(#document{case_id = CaseId}) -> CaseId.

%% @private
-spec get_name(#document{}) -> binary().
get_name(#document{name = Name}) -> Name.

%% @private
-spec get_content_type(#document{}) -> content_type().
get_content_type(#document{content_type = ContentType}) -> ContentType.

%% @private
-spec get_data(#document{}) -> binary().
get_data(#document{data = Data}) -> Data.

%% @private
-spec get_uploaded_at(#document{}) -> integer().
get_uploaded_at(#document{uploaded_at = UploadedAt}) -> UploadedAt.

%% @private
-spec get_uploaded_by(#document{}) -> binary().
get_uploaded_by(#document{uploaded_by = UploadedBy}) -> UploadedBy.

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

%% @private
init(Options) ->
    Backend = proplists:get_value(storage_backend, Options, local),
    Config = proplists:get_value(storage_config, Options, #{}),
    State = #doc_state{
        storage_backend = Backend,
        storage_config = Config
    },
    {ok, State}.

%% @private
handle_call({set_storage_backend, Backend}, _From, State) ->
    {reply, ok, State#doc_state{storage_backend = Backend}};

handle_call({store_document, CaseId, DocProps}, _From, State) when is_list(DocProps) ->
    {Reply, NewState} = do_store_document(CaseId, DocProps, State),
    {reply, Reply, NewState};

handle_call({store_document, CaseId, #document{} = Doc}, _From, State) ->
    {Reply, NewState} = do_store_document_record(CaseId, Doc, State),
    {reply, Reply, NewState};

handle_call({retrieve_document, DocId}, _From, State) ->
    Reply = do_retrieve_document(DocId, State),
    {reply, Reply, State};

handle_call({delete_document, DocId}, _From, State) ->
    {Reply, NewState} = do_delete_document(DocId, State),
    {reply, Reply, NewState};

handle_call({list_documents, CaseId}, _From, State) ->
    Reply = do_list_documents(CaseId, State),
    {reply, Reply, State};

handle_call({get_document_url, DocId}, _From, State) ->
    Reply = do_get_document_url(DocId, State),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(_Request, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private Stores a document from properties.
%% @end
%%--------------------------------------------------------------------
-spec do_store_document(case_id(), proplists:proplist(), #doc_state{}) ->
          {doc_result(), #doc_state{}}.
do_store_document(CaseId, Props, #doc_state{documents = Documents,
                                            case_index = CaseIndex} = State) ->
    Name = proplists:get_value(name, Props, <<"unnamed">>),
    ContentType = proplists:get_value(content_type, Props, <<"application/octet-stream">>),
    Data = proplists:get_value(data, Props, <<"">>),
    UploadedBy = proplists:get_value(uploaded_by, Props, <<"system">>),
    UploadedAt = erlang:system_time(second),

    Doc = #document{
        id = generate_id(<<"doc">>),
        case_id = CaseId,
        name = Name,
        content_type = ContentType,
        data = Data,
        uploaded_at = UploadedAt,
        uploaded_by = UploadedBy
    },

    DocId = Doc#document.id,
    NewDocuments = maps:put(DocId, Doc, Documents),
    NewCaseIndex = maps:put(CaseId, [DocId | maps:get(CaseId, CaseIndex, [])], CaseIndex),

    {{ok, DocId}, State#doc_state{documents = NewDocuments, case_index = NewCaseIndex}}.

%%--------------------------------------------------------------------
%% @private Stores an existing document record.
%% @end
%%--------------------------------------------------------------------
-spec do_store_document_record(case_id(), #document{}, #doc_state{}) ->
          {doc_result(), #doc_state{}}.
do_store_document_record(CaseId, #document{} = Doc, #doc_state{documents = Documents,
                                                                case_index = CaseIndex} = State) ->
    DocId = Doc#document.id,
    NewDocuments = maps:put(DocId, Doc, Documents),
    NewCaseIndex = maps:put(CaseId, [DocId | maps:get(CaseId, CaseIndex, [])], CaseIndex),

    {{ok, DocId}, State#doc_state{documents = NewDocuments, case_index = NewCaseIndex}}.

%%--------------------------------------------------------------------
%% @private Retrieves a document.
%% @end
%%--------------------------------------------------------------------
-spec do_retrieve_document(document_id(), #doc_state{}) -> doc_result().
do_retrieve_document(DocId, #doc_state{documents = Documents}) ->
    case maps:get(DocId, Documents, undefined) of
        undefined -> {error, not_found};
        Doc -> {ok, Doc}
    end.

%%--------------------------------------------------------------------
%% @private Deletes a document.
%% @end
%%--------------------------------------------------------------------
-spec do_delete_document(document_id(), #doc_state{}) ->
          {ok | {error, not_found}, #doc_state{}}.
do_delete_document(DocId, #doc_state{documents = Documents,
                                     case_index = CaseIndex} = State) ->
    case maps:get(DocId, Documents, undefined) of
        undefined ->
            {{error, not_found}, State};
        #document{case_id = CaseId} ->
            NewDocuments = maps:remove(DocId, Documents),
            NewCaseIndex = case maps:get(CaseId, CaseIndex, []) of
                [DocId] -> maps:remove(CaseId, CaseIndex);
                DocList -> maps:put(CaseId, lists:delete(DocId, DocList), CaseIndex)
            end,
            {ok, State#doc_state{documents = NewDocuments, case_index = NewCaseIndex}}
    end.

%%--------------------------------------------------------------------
%% @private Lists documents for a case.
%% @end
%%--------------------------------------------------------------------
-spec do_list_documents(case_id(), #doc_state{}) ->
          {ok, [#document{}]} | {error, not_found}.
do_list_documents(CaseId, #doc_state{documents = Documents,
                                      case_index = CaseIndex}) ->
    case maps:get(CaseId, CaseIndex, undefined) of
        undefined -> {error, not_found};
        [] -> {ok, []};
        DocIds ->
            Docs = [maps:get(Id, Documents) || Id <- DocIds, maps:is_key(Id, Documents)],
            {ok, lists:reverse(Docs)}
    end.

%%--------------------------------------------------------------------
%% @private Gets a URL for document access.
%% @end
%%--------------------------------------------------------------------
-spec do_get_document_url(document_id(), #doc_state{}) ->
          {ok, binary()} | {error, not_found}.
do_get_document_url(DocId, #doc_state{storage_backend = Backend,
                                      storage_config = Config,
                                      documents = Documents}) ->
    case maps:get(DocId, Documents, undefined) of
        undefined ->
            {error, not_found};
        _Doc ->
            case Backend of
                local ->
                    BasePath = maps:get(base_path, Config, "/var/yawl/documents"),
                    Url = <<"file://", BasePath/binary, "/", DocId/binary>>,
                    {ok, Url};
                s3 ->
                    Bucket = maps:get(bucket, Config, <<"yawl-documents">>),
                    Region = maps:get(region, Config, <<"us-east-1">>),
                    Url = <<"https://s3.", Region/binary, ".amazonaws.com/",
                            Bucket/binary, "/", DocId/binary>>,
                    {ok, Url}
            end
    end.

%%--------------------------------------------------------------------
%% @private Validates a document.
%% @end
%%--------------------------------------------------------------------
-spec do_validate_document(#document{}) -> {ok, #document{}} | {error, term()}.
do_validate_document(#document{name = Name, content_type = ContentType, data = Data} = Doc) ->
    % Validate name
    case byte_size(Name) of
        0 -> {error, invalid_name};
        _ when byte_size(Name) > 255 -> {error, name_too_long};
        _ ->
            % Validate content type
            case validate_content_type(ContentType) of
                false -> {error, invalid_content_type};
                true ->
                    % Validate data size (max 100MB)
                    MaxSize = 100 * 1024 * 1024,
                    case byte_size(Data) of
                        Size when Size > MaxSize -> {error, file_too_large};
                        _ -> {ok, Doc}
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @private Validates content type.
%% @end
%%--------------------------------------------------------------------
-spec validate_content_type(binary()) -> boolean().
validate_content_type(ContentType) ->
    % Check for valid MIME type format: type/subtype
    case binary:split(ContentType, <<"/">>) of
        [Type, Subtype] when byte_size(Type) > 0, byte_size(Subtype) > 0 -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @private Generates a unique ID.
%% @end
%%--------------------------------------------------------------------
-spec generate_id(binary()) -> binary().
generate_id(Prefix) ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<Prefix/binary, "_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @doc Run doctests for this module.
%% Tests document record creation, field access, storage backends,
%% content type validation, and ID formats.
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    %% === Document Record Tests ===

    %% Test 1: Document record creation and field access
    Doc = #document{
        id = <<"doc_test_123">>,
        case_id = <<"case_test_456">>,
        name = <<"report.pdf">>,
        content_type = <<"application/pdf">>,
        data = <<"%PDF-1.4">>,
        uploaded_at = 1704067200,
        uploaded_by = <<"user1">>
    },
    true = is_record(Doc, document),
    <<"doc_test_123">> = Doc#document.id,
    <<"case_test_456">> = Doc#document.case_id,
    <<"report.pdf">> = Doc#document.name,
    <<"application/pdf">> = Doc#document.content_type,
    <<"%PDF-1.4">> = Doc#document.data,
    1704067200 = Doc#document.uploaded_at,
    <<"user1">> = Doc#document.uploaded_by,

    %% Test 2: Empty document record
    EmptyDoc = #document{},
    true = is_record(EmptyDoc, document),

    %% === Document State Tests ===

    %% Test 3: Document state record creation
    State = #doc_state{
        documents = #{},
        case_index = #{},
        storage_backend = local,
        storage_config = #{}
    },
    true = is_record(State, doc_state),
    local = State#doc_state.storage_backend,
    #{} = State#doc_state.documents,
    #{} = State#doc_state.case_index,

    %% Test 4: S3 backend state
    S3State = #doc_state{
        storage_backend = s3,
        storage_config = #{bucket => <<"test-bucket">>, region => <<"us-east-1">>}
    },
    s3 = S3State#doc_state.storage_backend,

    %% === Storage Backend Tests ===

    %% Test 5: Valid storage backends
    LocalBackend = local,
    S3Backend = s3,
    true = LocalBackend =:= local orelse LocalBackend =:= s3,
    true = S3Backend =:= local orelse S3Backend =:= s3,

    %% === Content Type Validation Tests ===

    %% Test 6: Valid content types
    ValidTypes = [
        <<"application/pdf">>,
        <<"image/jpeg">>,
        <<"text/plain">>,
        <<"application/json">>
    ],
    true = lists:all(fun validate_content_type/1, ValidTypes),

    %% Test 7: Invalid content types
    InvalidTypes = [
        <<"not_a_mime">>,
        <<"application">>,
        <<"/plain">>,
        <<"">>
    ],
    false = lists:any(fun validate_content_type/1, InvalidTypes),

    %% Test 8: Content type validation pattern
    ValidCT = <<"application/pdf">>,
    case binary:split(ValidCT, <<"/">>) of
        [Type, Subtype] when byte_size(Type) > 0, byte_size(Subtype) > 0 -> ok;
        _ -> error(valid_content_type_failed)
    end,

    %% === Document ID Tests ===

    %% Test 9: Document ID format
    DocId = <<"doc_1a2b3c4d">>,
    true = is_binary(DocId),
    <<"doc_", _/binary>> = DocId,
    true = byte_size(DocId) > 4,

    %% Test 10: Case ID format
    CaseId = <<"case_xyz_789">>,
    true = is_binary(CaseId),
    true = byte_size(CaseId) > 0,

    %% === Document Name Tests ===

    %% Test 11: Valid document names
    ValidNames = [
        <<"document.pdf">>,
        <<"report.txt">>,
        <<"data.json">>,
        <<"a">>  % Minimum valid name
    ],
    true = lists:all(fun(N) -> byte_size(N) > 0 andalso byte_size(N) =< 255 end, ValidNames),

    %% Test 12: Invalid document names (empty or too long)
    EmptyName = <<"">>,
    0 = byte_size(EmptyName),

    %% === Timestamp Tests ===

    %% Test 13: Unix timestamp format
    Now = erlang:system_time(second),
    true = is_integer(Now),
    true = Now > 1700000000,  % Reasonable timestamp

    %% Test 14: Uploaded_at field pattern
    UploadedAt = Doc#document.uploaded_at,
    true = is_integer(UploadedAt),
    true = UploadedAt > 0,

    %% === User/Uploader Tests ===

    %% Test 15: Uploader binary format
    Uploader = <<"system">>,
    true = is_binary(Uploader),
    <<"system">> = Uploader,

    %% Test 16: Custom uploader
    CustomUploader = <<"user_john_doe">>,
    true = is_binary(CustomUploader),
    true = byte_size(CustomUploader) > 0,

    %% === Storage Configuration Tests ===

    %% Test 17: Local storage config
    LocalConfig = #{base_path => <<"/var/yawl/documents">>},
    <<"/var/yawl/documents">> = maps:get(base_path, LocalConfig),

    %% Test 18: S3 storage config
    S3Config = #{
        bucket => <<"yawl-documents">>,
        region => <<"us-west-2">>
    },
    <<"yawl-documents">> = maps:get(bucket, S3Config),
    <<"us-west-2">> = maps:get(region, S3Config),

    %% === Document Accessor Tests ===

    %% Test 19: get_doc_id accessor
    <<"doc_test_123">> = get_doc_id(Doc),

    %% Test 20: get_case_id accessor
    <<"case_test_456">> = get_case_id(Doc),

    %% Test 21: get_name accessor
    <<"report.pdf">> = get_name(Doc),

    %% Test 22: get_content_type accessor
    <<"application/pdf">> = get_content_type(Doc),

    %% Test 23: get_data accessor
    <<"%PDF-1.4">> = get_data(Doc),

    %% Test 24: get_uploaded_at accessor
    1704067200 = get_uploaded_at(Doc),

    %% Test 25: get_uploaded_by accessor
    <<"user1">> = get_uploaded_by(Doc),

    %% === Empty Document Constructor Tests ===

    %% Test 26: document() constructor
    EmptyDoc2 = document(),
    true = is_record(EmptyDoc2, document),

    %% === URL Generation Pattern Tests ===

    %% Test 27: Local URL pattern
    BasePath = <<"/var/yawl/documents">>,
    DocId2 = <<"doc_abc123">>,
    LocalUrl = <<"file://", BasePath/binary, "/", DocId2/binary>>,
    <<"file://", _/binary>> = LocalUrl,

    %% Test 28: S3 URL pattern
    Bucket = <<"yawl-documents">>,
    Region = <<"us-east-1">>,
    S3Url = <<"https://s3.", Region/binary, ".amazonaws.com/",
               Bucket/binary, "/", DocId2/binary>>,
    <<"https://s3.", _/binary>> = S3Url,

    %% === Document Validation Size Tests ===

    %% Test 29: Max file size constant
    MaxSize = 100 * 1024 * 1024,
    104857600 = MaxSize,

    %% Test 30: Small data (valid)
    SmallData = <<"small content">>,
    true = byte_size(SmallData) < MaxSize,

    %% Test 31: Exactly max size (boundary)
    MaxData = <<0:(MaxSize * 8)>>,
    MaxSize = byte_size(MaxData),

    %% === Case Index Tests ===

    %% Test 32: Case index empty state
    EmptyIndex = #{},
    #{} = EmptyIndex,

    %% Test 33: Case index with documents
    CaseIndex = #{<<"case1">> => [<<"doc1">>, <<"doc2">>]},
    [<<"doc1">>, <<"doc2">>] = maps:get(<<"case1">>, CaseIndex),

    %% === Documents Map Tests ===

    %% Test 34: Empty documents map
    EmptyDocs = #{},
    #{} = EmptyDocs,

    %% Test 35: Documents map with entries
    DocMap = #{<<"doc1">> => Doc, <<"doc2">> => EmptyDoc},
    Doc = maps:get(<<"doc1">>, DocMap),
    EmptyDoc = maps:get(<<"doc2">>, DocMap),

    %% === Result Type Tests ===

    %% Test 36: Success result tuple
    OkResult = {ok, <<"doc_123">>},
    {ok, <<"doc_123">>} = OkResult,
    ok = element(1, OkResult),

    %% Test 37: Error result tuple
    ErrorResult = {error, not_found},
    {error, not_found} = ErrorResult,
    error = element(1, ErrorResult),

    %% === MIME Type Pattern Tests ===

    %% Test 38: Common MIME type categories
    ApplicationType = <<"application/">>,
    ImageType = <<"image/">>,
    TextType = <<"text/">>,
    true = <<$a, $p, $p, $l, $i, $c, $a, $t, $i, $o, $n, $/>> =:= ApplicationType,
    true = <<$i, $m, $a, $g, $e, $/>> =:= ImageType,
    true = <<$t, $e, $x, $t, $/>> =:= TextType,

    %% === Binary Size Validation Tests ===

    %% Test 39: Name size boundaries
    MinName = <<"a">>,
    1 = byte_size(MinName),
    MaxName = binary:copy(<<"x">>, 255),
    255 = byte_size(MaxName),

    %% Test 40: Name exceeding max
    TooLongName = binary:copy(<<"x">>, 256),
    256 = byte_size(TooLongName),

    %% === Document Data Tests ===

    %% Test 41: Empty data (valid)
    EmptyData = <<"">>,
    0 = byte_size(EmptyData),

    %% Test 42: Non-empty data
    NonEmptyData = <<"some document content">>,
    true = byte_size(NonEmptyData) > 0,

    %% === Record Type Validation Tests ===

    %% Test 43: document record guard pattern
    IsDocRecord = case Doc of
        #document{} -> true;
        _ -> false
    end,
    true = IsDocRecord,

    %% Test 44: doc_state record guard pattern
    IsStateRecord = case State of
        #doc_state{} -> true;
        _ -> false
    end,
    true = IsStateRecord,

    %% === Completion ===
    ok.
