%% -*- erlang -*-
%%
%% CRE: REST Query and Filter Handlers
%%
%% HTTP handlers for workflow query and filter operations. Provides REST API
%% endpoints for querying workflows, cases, and work items with support for
%% filtering, pagination, and sorting.
%%
%% @end
%% -------------------------------------------------------------------

-module(rest_query).
-behaviour(cowboy_handler).

%%====================================================================
%% Exports
%%====================================================================

%% Cowboy handler callbacks
-export([init/2, terminate/3]).

%% Query handler API
-export([
    handle_query_workflows/2,
    handle_query_cases/2,
    handle_query_workitems/2,
    handle_query_logs/2
]).

%% Filter and query utilities
-export([
    apply_filters/2,
    apply_pagination/2,
    apply_sorting/2
]).

%%====================================================================
%% Types
%%====================================================================

-type filter_spec() :: #{
    status => binary() | [binary()],
    task => binary(),
    participant => binary(),
    created_after => integer(),
    created_before => integer(),
    updated_after => integer(),
    updated_before => integer(),
    assignee => binary(),
    priority => binary(),
    search => binary()
}.

-type query_result() :: #{
    items := list(),
    total := non_neg_integer(),
    offset := non_neg_integer(),
    limit := non_neg_integer(),
    timestamp := integer()
}.

-type sort_key() :: {Field :: atom(), Direction :: asc | desc}.

-export_type([filter_spec/0, query_result/0, sort_key/0]).

%%====================================================================
%% Cowboy Handler Callbacks
%%====================================================================

%% @doc Initialize handler - dispatch based on path and method
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    Req = dispatch_query(Method, Path, Req0),
    {ok, Req, State}.

%% @doc Terminate callback
-spec terminate(term(), cowboy_req:req(), term()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Query Dispatch
%%====================================================================

-spec dispatch_query(binary(), binary(), cowboy_req:req()) -> cowboy_req:req().
dispatch_query(<<"GET">>, <<"/api/workflows/query">>, Req0) ->
    handle_query_workflows(Req0, #{});
dispatch_query(<<"GET">>, <<"/api/cases/query">>, Req0) ->
    handle_query_cases(Req0, #{});
dispatch_query(<<"GET">>, <<"/api/workitems/query">>, Req0) ->
    handle_query_workitems(Req0, #{});
dispatch_query(<<"GET">>, <<"/api/logs/query">>, Req0) ->
    handle_query_logs(Req0, #{});
dispatch_query(<<"POST">>, <<"/api/workflows/filter">>, Req0) ->
    handle_filter_workflows(Req0);
dispatch_query(<<"POST">>, <<"/api/cases/filter">>, Req0) ->
    handle_filter_cases(Req0);
dispatch_query(<<"POST">>, <<"/api/workitems/filter">>, Req0) ->
    handle_filter_workitems(Req0);
dispatch_query(_Method, _Path, Req) ->
    reply_error(404, <<"Not found">>, Req).

%%====================================================================
%% Workflow Query Handler
%%====================================================================

-spec handle_query_workflows(cowboy_req:req(), map()) -> cowboy_req:req().
handle_query_workflows(Req0, _Opts) ->
    QS = cowboy_req:parse_qs(Req0),
    Filters = parse_query_filters(QS),
    Pagination = parse_pagination(QS),
    Sort = parse_sort(QS),

    case get_all_workflows() of
        {ok, Workflows} ->
            Filtered = apply_filters(Workflows, Filters),
            Sorted = apply_sorting(Filtered, Sort),
            {Offset, Limit} = Pagination,
            Paginated = apply_pagination(Sorted, {Offset, Limit}),
            Total = length(Filtered),
            Result = #{
                <<"items">> => Paginated,
                <<"total">> => Total,
                <<"offset">> => Offset,
                <<"limit">> => Limit,
                <<"timestamp">> => erlang:system_time(millisecond)
            },
            reply_json(200, Result, Req0);
        {error, Reason} ->
            reply_error(500, Reason, Req0)
    end.

%%====================================================================
%% Cases Query Handler
%%====================================================================

-spec handle_query_cases(cowboy_req:req(), map()) -> cowboy_req:req().
handle_query_cases(Req0, _Opts) ->
    QS = cowboy_req:parse_qs(Req0),
    Filters = parse_query_filters(QS),
    Pagination = parse_pagination(QS),
    Sort = parse_sort(QS),

    case get_all_cases() of
        {ok, Cases} ->
            Filtered = apply_filters(Cases, Filters),
            Sorted = apply_sorting(Filtered, Sort),
            {Offset, Limit} = Pagination,
            Paginated = apply_pagination(Sorted, {Offset, Limit}),
            Total = length(Filtered),
            Result = #{
                <<"items">> => Paginated,
                <<"total">> => Total,
                <<"offset">> => Offset,
                <<"limit">> => Limit,
                <<"timestamp">> => erlang:system_time(millisecond)
            },
            reply_json(200, Result, Req0);
        {error, Reason} ->
            reply_error(500, Reason, Req0)
    end.

%%====================================================================
%% Work Items Query Handler
%%====================================================================

-spec handle_query_workitems(cowboy_req:req(), map()) -> cowboy_req:req().
handle_query_workitems(Req0, _Opts) ->
    QS = cowboy_req:parse_qs(Req0),
    Filters = parse_query_filters(QS),
    Pagination = parse_pagination(QS),
    Sort = parse_sort(QS),

    case get_all_workitems() of
        {ok, WorkItems} ->
            Filtered = apply_filters(WorkItems, Filters),
            Sorted = apply_sorting(Filtered, Sort),
            {Offset, Limit} = Pagination,
            Paginated = apply_pagination(Sorted, {Offset, Limit}),
            Total = length(Filtered),
            Result = #{
                <<"items">> => Paginated,
                <<"total">> => Total,
                <<"offset">> => Offset,
                <<"limit">> => Limit,
                <<"timestamp">> => erlang:system_time(millisecond)
            },
            reply_json(200, Result, Req0);
        {error, Reason} ->
            reply_error(500, Reason, Req0)
    end.

%%====================================================================
%% Logs Query Handler
%%====================================================================

-spec handle_query_logs(cowboy_req:req(), map()) -> cowboy_req:req().
handle_query_logs(Req0, _Opts) ->
    QS = cowboy_req:parse_qs(Req0),
    Filters = parse_query_filters(QS),
    Pagination = parse_pagination(QS),
    Sort = parse_sort(QS),

    case get_all_logs() of
        {ok, Logs} ->
            Filtered = apply_filters(Logs, Filters),
            Sorted = apply_sorting(Filtered, Sort),
            {Offset, Limit} = Pagination,
            Paginated = apply_pagination(Sorted, {Offset, Limit}),
            Total = length(Filtered),
            Result = #{
                <<"items">> => Paginated,
                <<"total">> => Total,
                <<"offset">> => Offset,
                <<"limit">> => Limit,
                <<"timestamp">> => erlang:system_time(millisecond)
            },
            reply_json(200, Result, Req0);
        {error, Reason} ->
            reply_error(500, Reason, Req0)
    end.

%%====================================================================
%% POST Filter Handlers
%%====================================================================

-spec handle_filter_workflows(cowboy_req:req()) -> cowboy_req:req().
handle_filter_workflows(Req0) ->
    case cowboy_req:read_body(Req0) of
        {ok, Body, Req} ->
            case parse_json(Body) of
                {ok, FilterMap} ->
                    Filters = FilterMap,
                    Pagination = extract_pagination(FilterMap),
                    Sort = extract_sort(FilterMap),
                    case get_all_workflows() of
                        {ok, Workflows} ->
                            Filtered = apply_filters(Workflows, Filters),
                            Sorted = apply_sorting(Filtered, Sort),
                            {Offset, Limit} = Pagination,
                            Paginated = apply_pagination(Sorted, {Offset, Limit}),
                            Total = length(Filtered),
                            Result = #{
                                <<"items">> => Paginated,
                                <<"total">> => Total,
                                <<"offset">> => Offset,
                                <<"limit">> => Limit,
                                <<"timestamp">> => erlang:system_time(millisecond)
                            },
                            reply_json(200, Result, Req);
                        {error, Reason} ->
                            reply_error(500, Reason, Req)
                    end;
                {error, _} ->
                    reply_error(400, <<"Invalid JSON">>, Req)
            end;
        {error, _} ->
            reply_error(400, <<"Failed to read body">>, Req)
    end.

-spec handle_filter_cases(cowboy_req:req()) -> cowboy_req:req().
handle_filter_cases(Req0) ->
    case cowboy_req:read_body(Req0) of
        {ok, Body, Req} ->
            case parse_json(Body) of
                {ok, FilterMap} ->
                    Filters = FilterMap,
                    Pagination = extract_pagination(FilterMap),
                    Sort = extract_sort(FilterMap),
                    case get_all_cases() of
                        {ok, Cases} ->
                            Filtered = apply_filters(Cases, Filters),
                            Sorted = apply_sorting(Filtered, Sort),
                            {Offset, Limit} = Pagination,
                            Paginated = apply_pagination(Sorted, {Offset, Limit}),
                            Total = length(Filtered),
                            Result = #{
                                <<"items">> => Paginated,
                                <<"total">> => Total,
                                <<"offset">> => Offset,
                                <<"limit">> => Limit,
                                <<"timestamp">> => erlang:system_time(millisecond)
                            },
                            reply_json(200, Result, Req);
                        {error, Reason} ->
                            reply_error(500, Reason, Req)
                    end;
                {error, _} ->
                    reply_error(400, <<"Invalid JSON">>, Req)
            end;
        {error, _} ->
            reply_error(400, <<"Failed to read body">>, Req)
    end.

-spec handle_filter_workitems(cowboy_req:req()) -> cowboy_req:req().
handle_filter_workitems(Req0) ->
    case cowboy_req:read_body(Req0) of
        {ok, Body, Req} ->
            case parse_json(Body) of
                {ok, FilterMap} ->
                    Filters = FilterMap,
                    Pagination = extract_pagination(FilterMap),
                    Sort = extract_sort(FilterMap),
                    case get_all_workitems() of
                        {ok, WorkItems} ->
                            Filtered = apply_filters(WorkItems, Filters),
                            Sorted = apply_sorting(Filtered, Sort),
                            {Offset, Limit} = Pagination,
                            Paginated = apply_pagination(Sorted, {Offset, Limit}),
                            Total = length(Filtered),
                            Result = #{
                                <<"items">> => Paginated,
                                <<"total">> => Total,
                                <<"offset">> => Offset,
                                <<"limit">> => Limit,
                                <<"timestamp">> => erlang:system_time(millisecond)
                            },
                            reply_json(200, Result, Req);
                        {error, Reason} ->
                            reply_error(500, Reason, Req)
                    end;
                {error, _} ->
                    reply_error(400, <<"Invalid JSON">>, Req)
            end;
        {error, _} ->
            reply_error(400, <<"Failed to read body">>, Req)
    end.

%%====================================================================
%% Filter and Query Utilities
%%====================================================================

%% @doc Apply filters to a list of items
-spec apply_filters(list(), filter_spec()) -> list().
apply_filters(Items, Filters) when is_list(Items) ->
    lists:filter(
        fun(Item) ->
            matches_all_filters(Item, Filters)
        end,
        Items
    ).

%% @doc Check if an item matches all filters
-spec matches_all_filters(map(), filter_spec()) -> boolean().
matches_all_filters(Item, Filters) when is_map(Item), is_map(Filters) ->
    lists:all(
        fun({Key, Value}) ->
            match_filter(Item, Key, Value)
        end,
        maps:to_list(Filters)
    ).

%% @doc Match a single filter criterion
-spec match_filter(map(), atom() | binary(), any()) -> boolean().
match_filter(Item, status, Values) when is_list(Values) ->
    Status = maps:get(<<"status">>, Item, maps:get(status, Item, undefined)),
    lists:member(Status, Values);
match_filter(Item, status, Value) ->
    Status = maps:get(<<"status">>, Item, maps:get(status, Item, undefined)),
    Status =:= Value;
match_filter(Item, task, Value) ->
    Task = maps:get(<<"task">>, Item, maps:get(task, Item, undefined)),
    Task =:= Value;
match_filter(Item, participant, Value) ->
    Participant = maps:get(<<"participant">>, Item, maps:get(participant, Item, undefined)),
    Participant =:= Value;
match_filter(Item, assignee, Value) ->
    Assignee = maps:get(<<"assigned_to">>, Item, maps:get(assigned_to, Item, undefined)),
    Assignee =:= Value;
match_filter(Item, created_after, Timestamp) when is_integer(Timestamp) ->
    Created = maps:get(<<"created_at">>, Item, maps:get(created_at, Item, 0)),
    is_integer(Created) andalso Created >= Timestamp;
match_filter(Item, created_before, Timestamp) when is_integer(Timestamp) ->
    Created = maps:get(<<"created_at">>, Item, maps:get(created_at, Item, 0)),
    is_integer(Created) andalso Created =< Timestamp;
match_filter(Item, updated_after, Timestamp) when is_integer(Timestamp) ->
    Updated = maps:get(<<"updated_at">>, Item, maps:get(updated_at, Item, 0)),
    is_integer(Updated) andalso Updated >= Timestamp;
match_filter(Item, updated_before, Timestamp) when is_integer(Timestamp) ->
    Updated = maps:get(<<"updated_at">>, Item, maps:get(updated_at, Item, 0)),
    is_integer(Updated) andalso Updated =< Timestamp;
match_filter(Item, priority, Value) ->
    Priority = maps:get(<<"priority">>, Item, maps:get(priority, Item, undefined)),
    Priority =:= Value;
match_filter(Item, search, SearchTerm) when is_binary(SearchTerm) ->
    search_item(Item, SearchTerm);
match_filter(Item, _, _) ->
    %% Unknown filters pass through (don't filter)
    true.

%% @doc Search within an item (case-insensitive substring match)
-spec search_item(map(), binary()) -> boolean().
search_item(Item, SearchTerm) ->
    LowerTerm = string:lowercase(binary_to_list(SearchTerm)),
    lists:any(
        fun(Value) ->
            case Value of
                V when is_binary(V) ->
                    string:find(string:lowercase(binary_to_list(V)), LowerTerm) =/= nomatch;
                V when is_list(V) ->
                    string:find(string:lowercase(V), LowerTerm) =/= nomatch;
                _ -> false
            end
        end,
        maps:values(Item)
    ).

%% @doc Apply sorting to a list of items
-spec apply_sorting(list(), [sort_key()]) -> list().
apply_sorting(Items, []) ->
    Items;
apply_sorting(Items, SortKeys) when is_list(Items), is_list(SortKeys) ->
    lists:sort(
        fun(A, B) ->
            compare_items(A, B, SortKeys)
        end,
        Items
    ).

%% @doc Compare two items based on sort keys
-spec compare_items(map(), map(), [sort_key()]) -> boolean().
compare_items(_A, _B, []) ->
    true;
compare_items(A, B, [{Field, Direction} | Rest]) ->
    ValA = maps:get(to_binary(Field), A, maps:get(Field, A, undefined)),
    ValB = maps:get(to_binary(Field), B, maps:get(Field, B, undefined)),
    Cmp = compare_values(ValA, ValB),
    case {Cmp, Direction} of
        {equal, _} ->
            compare_items(A, B, Rest);
        {less, asc} -> true;
        {less, desc} -> false;
        {greater, asc} -> false;
        {greater, desc} -> true
    end.

%% @doc Compare two values
-spec compare_values(any(), any()) -> less | equal | greater.
compare_values(undefined, undefined) -> equal;
compare_values(undefined, _) -> less;
compare_values(_, undefined) -> greater;
compare_values(A, B) when A < B -> less;
compare_values(A, B) when A =:= B -> equal;
compare_values(_, _) -> greater.

%% @doc Apply pagination to a list
-spec apply_pagination(list(), {Offset :: non_neg_integer(), Limit :: non_neg_integer()}) -> list().
apply_pagination(Items, {Offset, Limit}) ->
    lists:sublist(Items, Offset + 1, Limit).

%%====================================================================
%% Query Parameter Parsing
%%====================================================================

-spec parse_query_filters(list()) -> filter_spec().
parse_query_filters(QS) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            Atom = binary_to_atom(Key, utf8),
            case Atom of
                status ->
                    Acc#{status => Value};
                task ->
                    Acc#{task => Value};
                participant ->
                    Acc#{participant => Value};
                assignee ->
                    Acc#{assignee => Value};
                created_after ->
                    case catch binary_to_integer(Value) of
                        I when is_integer(I) -> Acc#{created_after => I};
                        _ -> Acc
                    end;
                created_before ->
                    case catch binary_to_integer(Value) of
                        I when is_integer(I) -> Acc#{created_before => I};
                        _ -> Acc
                    end;
                updated_after ->
                    case catch binary_to_integer(Value) of
                        I when is_integer(I) -> Acc#{updated_after => I};
                        _ -> Acc
                    end;
                updated_before ->
                    case catch binary_to_integer(Value) of
                        I when is_integer(I) -> Acc#{updated_before => I};
                        _ -> Acc
                    end;
                priority ->
                    Acc#{priority => Value};
                search ->
                    Acc#{search => Value};
                _ ->
                    Acc
            end
        catch
            _:_ -> Acc
        end,
        #{},
        QS
    ).

-spec parse_pagination(list()) -> {Offset :: non_neg_integer(), Limit :: non_neg_integer()}.
parse_pagination(QS) ->
    Offset = case lists:keyfind(<<"offset">>, 1, QS) of
        {_, O} -> safe_binary_to_integer(O, 0);
        false -> 0
    end,
    Limit = case lists:keyfind(<<"limit">>, 1, QS) of
        {_, L} -> safe_binary_to_integer(L, 50);
        false -> 50
    end,
    {Offset, min(Limit, 1000)}.

-spec parse_sort(list()) -> [sort_key()].
parse_sort(QS) ->
    case lists:keyfind(<<"sort">>, 1, QS) of
        {_, SortStr} ->
            parse_sort_string(SortStr);
        false ->
            []
    end.

-spec parse_sort_string(binary()) -> [sort_key()].
parse_sort_string(SortStr) ->
    Fields = binary:split(SortStr, <<",">>, [global]),
    lists:filtermap(
        fun(Field) ->
            case binary:split(Field, <<":">>) of
                [F, <<"asc">>] ->
                    {true, {binary_to_atom(F, utf8), asc}};
                [F, <<"desc">>] ->
                    {true, {binary_to_atom(F, utf8), desc}};
                [F] ->
                    {true, {binary_to_atom(F, utf8), asc}};
                _ ->
                    false
            end
        end,
        Fields
    ).

-spec extract_pagination(map()) -> {Offset :: non_neg_integer(), Limit :: non_neg_integer()}.
extract_pagination(Map) ->
    Offset = case maps:get(<<"offset">>, Map, maps:get(offset, Map, 0)) of
        O when is_integer(O) -> O;
        _ -> 0
    end,
    Limit = case maps:get(<<"limit">>, Map, maps:get(limit, Map, 50)) of
        L when is_integer(L) -> min(L, 1000);
        _ -> 50
    end,
    {Offset, Limit}.

-spec extract_sort(map()) -> [sort_key()].
extract_sort(Map) ->
    case maps:get(<<"sort">>, Map, maps:get(sort, Map, [])) of
        SortList when is_list(SortList) ->
            lists:filtermap(
                fun
                    (#{<<"field">> := F, <<"direction">> := D}) when D =:= <<"asc">>, is_binary(F) ->
                        {true, {binary_to_atom(F, utf8), asc}};
                    (#{<<"field">> := F, <<"direction">> := D}) when D =:= <<"desc">>, is_binary(F) ->
                        {true, {binary_to_atom(F, utf8), desc}};
                    (_) ->
                        false
                end,
                SortList
            );
        _ -> []
    end.

%%====================================================================
%% Data Retrieval Functions
%%====================================================================

%% @doc Get all workflows (stub - integrate with actual backend)
-spec get_all_workflows() -> {ok, list()} | {error, term()}.
get_all_workflows() ->
    try
        case whereis(yawl_registry) of
            undefined -> {ok, []};
            _ ->
                Cases = [case_to_workflow_map(CaseId) || {CaseId, _} <- yawl_registry:list()],
                {ok, Cases}
        end
    catch
        _:_ -> {ok, []}
    end.

%% @doc Get all cases (stub - integrate with actual backend)
-spec get_all_cases() -> {ok, list()} | {error, term()}.
get_all_cases() ->
    try
        case whereis(yawl_registry) of
            undefined -> {ok, []};
            _ ->
                Cases = [case_to_case_map(CaseId) || {CaseId, _} <- yawl_registry:list()],
                {ok, Cases}
        end
    catch
        _:_ -> {ok, []}
    end.

%% @doc Get all work items (stub - integrate with actual backend)
-spec get_all_workitems() -> {ok, list()} | {error, term()}.
get_all_workitems() ->
    {ok, []}.

%% @doc Get all logs (stub - integrate with actual backend)
-spec get_all_logs() -> {ok, list()} | {error, term()}.
get_all_logs() ->
    {ok, []}.

%%====================================================================
%% Data Mapping Functions
%%====================================================================

-spec case_to_workflow_map(binary()) -> map().
case_to_workflow_map(CaseId) ->
    #{
        <<"id">> => CaseId,
        <<"status">> => <<"running">>,
        <<"created_at">> => erlang:system_time(millisecond)
    }.

-spec case_to_case_map(binary()) -> map().
case_to_case_map(CaseId) ->
    #{
        <<"case_id">> => CaseId,
        <<"status">> => <<"running">>,
        <<"created_at">> => erlang:system_time(millisecond),
        <<"updated_at">> => erlang:system_time(millisecond)
    }.

%%====================================================================
%% Response Utilities
%%====================================================================

-spec reply_json(integer(), map(), cowboy_req:req()) -> cowboy_req:req().
reply_json(Status, Body, Req) ->
    cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Body),
        Req).

-spec reply_error(integer(), binary() | term(), cowboy_req:req()) -> cowboy_req:req().
reply_error(Status, ErrorMsg, Req) ->
    Body = #{
        <<"error">> => to_binary(ErrorMsg),
        <<"timestamp">> => erlang:system_time(millisecond)
    },
    reply_json(Status, Body, Req).

-spec parse_json(binary()) -> {ok, map()} | {error, term()}.
parse_json(Body) ->
    try
        {ok, jsx:decode(Body, [return_maps])}
    catch
        _:Error -> {error, Error}
    end.

-spec to_binary(any()) -> binary().
to_binary(X) when is_binary(X) -> X;
to_binary(X) when is_atom(X) -> atom_to_binary(X, utf8);
to_binary(X) when is_integer(X) -> integer_to_binary(X);
to_binary(X) when is_list(X) -> list_to_binary(X);
to_binary(X) -> term_to_binary(X).

-spec safe_binary_to_integer(binary(), integer()) -> integer().
safe_binary_to_integer(Binary, Default) ->
    try
        binary_to_integer(Binary)
    catch
        _:_ -> Default
    end.
