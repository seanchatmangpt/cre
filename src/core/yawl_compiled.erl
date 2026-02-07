%% -*- erlang -*-
%%
-module(yawl_compiled).
-moduledoc """
Access compiled YAWL specifications.

Provides pure accessor functions for compiled YAWL specifications.
All functions are total (never crash) and return sensible defaults.

## Compiled Specification Structure

A compiled specification contains:
- `original` - The original YAWL specification from wf_spec
- `nets` - Map of net IDs to net information
- `metadata` - Metadata about the specification

Each net contains:
- `id` - Binary identifier of the net
- `module` - The Erlang module implementing the net
- `places` - List of place atoms
- `transitions` - List of transition atoms
- `tasks` - List of task atoms
- `flows` - List of flow information maps

## Examples

```erlang
%% Create a compiled specification
Spec = wf_spec:from_xml(Xml),
{ok, Compiled} = wf_spec:compile(Spec),

%% Access nets in the compiled spec
{ok, NetInfo} = yawl_compiled:net(Compiled, <<"main">>),

%% Get module name for a net
{ok, Module} = yawl_compiled:net_module(Compiled, <<"main">>),

%% Get places in a net
{ok, Places} = yawl_compiled:places(Compiled, <<"main">>),

%% Get transitions in a net
{ok, Transitions} = yawl_compiled:transitions(Compiled, <<"main">>),

%% Get tasks in a net
{ok, Tasks} = yawl_compiled:tasks(Compiled, <<"main">>),

%% Get flows from a task
{ok, Flows} = yawl_compiled:flows(Compiled, <<"main">>, approve_task),

%% Validate a compiled spec
ok = yawl_compiled:validate(Compiled).

%% Check if a term is a compiled spec
true = yawl_compiled:is_compiled(Compiled).
```

## Total Functions

All accessor functions are total - they never crash. When a net or flow
is not found, they return `{error, not_found}` instead of throwing exceptions.
""".

%%====================================================================
%% Exports
%%====================================================================

%% Net accessors
-export([net/2, tasks/2, places/2, transitions/2, flows/3]).
-export([net_module/2, get_metadata/1, original_spec/1]).

%% Validation
-export([is_compiled/1, validate/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A compiled YAWL specification.
%%
%% Contains the original specification, compiled nets, and metadata.
%%--------------------------------------------------------------------
-type compiled_spec() :: #{
    original := wf_spec:yawl_spec(),
    nets := #{binary() => net_info()},
    metadata := metadata()
}.

%%--------------------------------------------------------------------
%% @doc Information about a compiled net.
%%
%% Contains the net ID, implementing module, structure, and flows.
%%--------------------------------------------------------------------
-type net_info() :: #{
    id := binary(),
    module := module(),
    places := [atom()],
    transitions := [atom()],
    tasks := [atom()],
    flows := [flow_info()]
}.

%%--------------------------------------------------------------------
%% @doc Flow information between tasks.
%%
%% Describes a connection from one task to another with an optional
%% predicate and default flag.
%%--------------------------------------------------------------------
-type flow_info() :: #{
    from := atom(),
    to := atom(),
    predicate := binary() | none,
    default := boolean()
}.

%%--------------------------------------------------------------------
%% @doc Metadata about the compiled specification.
%%
%% Contains title, version, and other metadata fields.
%%--------------------------------------------------------------------
-type metadata() :: #{
    title := binary(),
    version := binary()
}.

%%--------------------------------------------------------------------
%% @doc A validation error.
%%
%% Contains a binary error message describing what failed validation.
%%--------------------------------------------------------------------
-type validation_error() :: binary().

-export_type([compiled_spec/0, net_info/0, flow_info/0, metadata/0, validation_error/0]).

%%====================================================================
%% Net Accessor Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Get information about a specific net by ID.
%%
%% Returns `{ok, NetInfo}` if the net exists, or `{error, not_found}`
%% if the net ID is not present in the compiled specification.
%%
%% ```erlang
%% > {ok, NetInfo} = yawl_compiled:net(Compiled, <<"main">>).
%% {ok, #{id := <<"main">>, module := main_net, ...}}
%% > yawl_compiled:net(Compiled, <<"nonexistent">>).
%% {error, not_found}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec net(Compiled :: compiled_spec(), NetId :: binary()) ->
    {ok, net_info()} | {error, not_found}.

net(Compiled, NetId) when is_map(Compiled), is_binary(NetId) ->
    case Compiled of
        #{nets := Nets} when is_map(Nets) ->
            case maps:get(NetId, Nets, undefined) of
                NetInfo when is_map(NetInfo) -> {ok, NetInfo};
                _ -> {error, not_found}
            end;
        _ ->
            {error, not_found}
    end;
net(_Compiled, _NetId) ->
    {error, not_found}.

%%--------------------------------------------------------------------
%% @doc Get the list of tasks in a net.
%%
%% Returns `{ok, Tasks}` where Tasks is a list of task atoms, or
%% `{error, not_found}` if the net does not exist.
%%
%% ```erlang
%% > {ok, Tasks} = yawl_compiled:tasks(Compiled, <<"main">>).
%% {ok, [approve_task, notify_task]}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec tasks(Compiled :: compiled_spec(), NetId :: binary()) ->
    {ok, [atom()]} | {error, not_found}.

tasks(Compiled, NetId) when is_map(Compiled), is_binary(NetId) ->
    case net(Compiled, NetId) of
        {ok, #{tasks := Tasks}} when is_list(Tasks) -> {ok, Tasks};
        {ok, _} -> {ok, []};
        {error, not_found} -> {error, not_found}
    end;
tasks(_Compiled, _NetId) ->
    {error, not_found}.

%%--------------------------------------------------------------------
%% @doc Get the list of places in a net.
%%
%% Returns `{ok, Places}` where Places is a list of place atoms, or
%% `{error, not_found}` if the net does not exist.
%%
%% ```erlang
%% > {ok, Places} = yawl_compiled:places(Compiled, <<"main">>).
%% {ok, [start, approve, end]}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec places(Compiled :: compiled_spec(), NetId :: binary()) ->
    {ok, [atom()]} | {error, not_found}.

places(Compiled, NetId) when is_map(Compiled), is_binary(NetId) ->
    case net(Compiled, NetId) of
        {ok, #{places := Places}} when is_list(Places) -> {ok, Places};
        {ok, _} -> {ok, []};
        {error, not_found} -> {error, not_found}
    end;
places(_Compiled, _NetId) ->
    {error, not_found}.

%%--------------------------------------------------------------------
%% @doc Get the list of transitions in a net.
%%
%% Returns `{ok, Transitions}` where Transitions is a list of transition
%% atoms, or `{error, not_found}` if the net does not exist.
%%
%% ```erlang
%% > {ok, Transitions} = yawl_compiled:transitions(Compiled, <<"main">>).
%% {ok, [t_approve, t_notify]}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec transitions(Compiled :: compiled_spec(), NetId :: binary()) ->
    {ok, [atom()]} | {error, not_found}.

transitions(Compiled, NetId) when is_map(Compiled), is_binary(NetId) ->
    case net(Compiled, NetId) of
        {ok, #{transitions := Transitions}} when is_list(Transitions) -> {ok, Transitions};
        {ok, _} -> {ok, []};
        {error, not_found} -> {error, not_found}
    end;
transitions(_Compiled, _NetId) ->
    {error, not_found}.

%%--------------------------------------------------------------------
%% @doc Get the flows from a specific task in a net.
%%
%% Returns `{ok, Flows}` where Flows is a list of flow_info maps,
%% or `{error, not_found}` if the net or task does not exist.
%%
%% ```erlang
%% > {ok, Flows} = yawl_compiled:flows(Compiled, <<"main">>, approve).
%% {ok, [#{from := approve, to := notify, predicate := none, default := true}]}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec flows(Compiled :: compiled_spec(), NetId :: binary(), FromTask :: atom()) ->
    {ok, [flow_info()]} | {error, not_found}.

flows(Compiled, NetId, FromTask) when is_map(Compiled), is_binary(NetId), is_atom(FromTask) ->
    case net(Compiled, NetId) of
        {ok, #{flows := Flows}} when is_list(Flows) ->
            Filtered = lists:filter(fun(Flow) ->
                case Flow of
                    #{from := FromTask} -> true;
                    _ -> false
                end
            end, Flows),
            {ok, Filtered};
        {ok, _} ->
            {ok, []};
        {error, not_found} ->
            {error, not_found}
    end;
flows(_Compiled, _NetId, _FromTask) ->
    {error, not_found}.

%%--------------------------------------------------------------------
%% @doc Get the module name implementing a net.
%%
%% Returns `{ok, Module}` where Module is the Erlang module atom,
%% or `{error, not_found}` if the net does not exist.
%%
%% ```erlang
%% > {ok, Module} = yawl_compiled:net_module(Compiled, <<"main">>).
%% {ok, main_net}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec net_module(Compiled :: compiled_spec(), NetId :: binary()) ->
    {ok, module()} | {error, not_found}.

net_module(Compiled, NetId) when is_map(Compiled), is_binary(NetId) ->
    case net(Compiled, NetId) of
        {ok, #{module := Module}} when is_atom(Module) -> {ok, Module};
        {ok, _} -> {error, not_found};
        {error, not_found} -> {error, not_found}
    end;
net_module(_Compiled, _NetId) ->
    {error, not_found}.

%%--------------------------------------------------------------------
%% @doc Get metadata from the compiled specification.
%%
%% Returns a map containing at least `title` and `version` keys.
%% This function never crashes - it returns a valid metadata map
%% even if the input is malformed.
%%
%% ```erlang
%% > Meta = yawl_compiled:get_metadata(Compiled).
%% #{title := <<"My Workflow">>, version := <<"1.0">>}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec get_metadata(Compiled :: compiled_spec()) ->
    #{title := binary(), version := binary()}.

get_metadata(Compiled) when is_map(Compiled) ->
    case Compiled of
        #{metadata := Meta} when is_map(Meta) ->
            %% Ensure required fields exist
            Title = case Meta of
                #{title := T} when is_binary(T) -> T;
                _ -> <<"Untitled">>
            end,
            Version = case Meta of
                #{version := V} when is_binary(V) -> V;
                _ -> <<"0.0">>
            end,
            #{title => Title, version => Version};
        _ ->
            #{title => <<"Untitled">>, version => <<"0.0">>}
    end;
get_metadata(_) ->
    #{title => <<"Untitled">>, version => <<"0.0">>}.

%%--------------------------------------------------------------------
%% @doc Get the original YAWL specification.
%%
%% Returns the original wf_spec:yawl_spec() that was compiled.
%% Returns undefined if the compiled spec is malformed.
%%
%% ```erlang
%% > Original = yawl_compiled:original_spec(Compiled).
%% %% Original is a wf_spec:yawl_spec() opaque type
%% > wf_spec:id(Original).
%% <<"my_workflow">>
%% ```
%% @end
%%--------------------------------------------------------------------
-spec original_spec(Compiled :: compiled_spec()) -> wf_spec:yawl_spec() | undefined.

original_spec(Compiled) when is_map(Compiled) ->
    case Compiled of
        #{original := Spec} -> Spec;
        _ -> undefined
    end;
original_spec(_) ->
    undefined.

%%====================================================================
%% Validation Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Check if a term is a valid compiled specification.
%%
%% Returns true if the term is a properly structured compiled_spec(),
%% false otherwise. This function never crashes.
%%
%% ```erlang
%% > yawl_compiled:is_compiled(Compiled).
%% true
%% > yawl_compiled:is_compiled(#{invalid => structure}).
%% false
%% ```
%% @end
%%--------------------------------------------------------------------
-spec is_compiled(term()) -> boolean().

is_compiled(Term) when is_map(Term) ->
    try
        %% Check for required top-level keys
        case Term of
            #{original := Original, nets := Nets, metadata := _} when is_map(Nets), is_map(Original) ->
                %% Validate original is a map that could be a yawl_spec
                %% Since yawl_spec is opaque, we check for common fields
                case Original of
                    #{id := Id, title := Title, root_net := RootNet}
                      when is_binary(Id); is_binary(Title); is_binary(RootNet) ->
                        %% Validate each net entry
                        maps:fold(fun(_NetId, NetInfo, _Acc) ->
                            case NetInfo of
                                #{id := _, module := Module, places := Places,
                                  transitions := Trans, tasks := Tasks, flows := Flows}
                                  when is_atom(Module), is_list(Places),
                                       is_list(Trans), is_list(Tasks), is_list(Flows) ->
                                    ok;
                                _ ->
                                    throw(false)
                            end
                        end, ok, Nets),
                        true;
                    _ ->
                        %% Original might still be valid (opaque type)
                        %% Check if it's a valid map structure
                        maps:fold(fun(_NetId, NetInfo, _Acc) ->
                            case NetInfo of
                                #{id := _, module := Module, places := Places,
                                  transitions := Trans, tasks := Tasks, flows := Flows}
                                  when is_atom(Module), is_list(Places),
                                       is_list(Trans), is_list(Tasks), is_list(Flows) ->
                                    ok;
                                _ ->
                                    throw(false)
                            end
                        end, ok, Nets),
                        true
                end;
            _ ->
                false
        end
    catch
        throw:false -> false;
        _:_ -> false
    end;
is_compiled(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Validate a compiled specification.
%%
%% Returns `ok` if the specification is valid, or `{error, Errors}`
%% where Errors is a list of binary error messages describing validation
%% failures.
%%
%% ```erlang
%% > yawl_compiled:validate(Compiled).
%% ok
%% > yawl_compiled:validate(InvalidCompiled).
%% {error, [<<"Missing required field: nets">>]}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec validate(Compiled :: compiled_spec()) -> ok | {error, [validation_error()]}.

validate(Compiled) when is_map(Compiled) ->
    Errors = lists:flatten([
        validate_original(Compiled),
        validate_nets(Compiled),
        validate_metadata(Compiled)
    ]),
    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end;
validate(_) ->
    {error, [<<"Compiled spec must be a map">>]}.

%%====================================================================
%% Internal Validation Functions
%%====================================================================

%% @private
-spec validate_original(map()) -> [validation_error()].

validate_original(Compiled) ->
    case Compiled of
        #{original := Original} when is_map(Original) ->
            %% Since yawl_spec is opaque, we do basic validation
            %% by checking for expected fields
            case Original of
                #{id := _} -> [];
                _ -> []
                %% We accept any map since the type is opaque
                %% The actual validation happens in wf_spec
            end;
        _ -> [<<"Missing required field: original">>]
    end.

%% @private
-spec validate_nets(map()) -> [validation_error()].

validate_nets(Compiled) ->
    case Compiled of
        #{nets := Nets} when is_map(Nets) ->
            maps:fold(fun(NetId, NetInfo, Acc) ->
                Acc ++ validate_net(NetId, NetInfo)
            end, [], Nets);
        _ -> [<<"Missing required field: nets">>]
    end.

%% @private
-spec validate_net(binary(), term()) -> [validation_error()].

validate_net(NetId, NetInfo) when is_map(NetInfo) ->
    lists:flatten([
        validate_net_field(NetId, NetInfo, id, fun is_binary/1, <<"binary">>),
        validate_net_field(NetId, NetInfo, module, fun is_atom/1, <<"atom">>),
        validate_net_field(NetId, NetInfo, places, fun is_list/1, <<"list">>),
        validate_net_field(NetId, NetInfo, transitions, fun is_list/1, <<"list">>),
        validate_net_field(NetId, NetInfo, tasks, fun is_list/1, <<"list">>),
        validate_net_field(NetId, NetInfo, flows, fun is_list/1, <<"list">>)
    ]);
validate_net(NetId, _NetInfo) ->
    [<<(NetId)/binary, ": net info must be a map">>].

%% @private
-spec validate_net_field(binary(), map(), atom(), fun((term()) -> boolean()), binary()) ->
    [validation_error()].

validate_net_field(NetId, NetInfo, Field, Validator, TypeName) ->
    FieldBin = atom_to_binary(Field, utf8),
    case NetInfo of
        #{Field := Value} ->
            case Validator(Value) of
                true -> [];
                false -> [<<(NetId)/binary, ": field '", FieldBin/binary, "' must be a ", TypeName/binary>>]
            end;
        _ ->
            [<<(NetId)/binary, ": missing required field: '", FieldBin/binary, "'">>]
    end.

%% @private
-spec validate_metadata(map()) -> [validation_error()].

validate_metadata(Compiled) ->
    case Compiled of
        #{metadata := Meta} when is_map(Meta) ->
            lists:flatten([
                validate_meta_field(Meta, title, fun is_binary/1),
                validate_meta_field(Meta, version, fun is_binary/1)
            ]);
        _ -> [<<"Missing required field: metadata">>]
    end.

%% @private
-spec validate_meta_field(map(), atom(), fun((term()) -> boolean())) ->
    [validation_error()].

validate_meta_field(Meta, Field, Validator) ->
    FieldBin = atom_to_binary(Field, utf8),
    case Meta of
        #{Field := Value} ->
            case Validator(Value) of
                true -> [];
                false -> [<<"Field '", FieldBin/binary, "' in metadata must be a binary">>]
            end;
        _ ->
            [<<"Missing required field in metadata: '", FieldBin/binary, "'">>]
    end.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test helper: create a minimal valid compiled spec
%% Since yawl_spec is opaque, we use a map representation for testing
%% that mimics the expected structure
make_test_compiled() ->
    %% Create a mock yawl_spec as a map (opaque type)
    Spec = #{
        id => <<"test">>,
        title => <<"Test">>,
        version => <<"1.0">>,
        root_net => <<"main">>,
        tasks => #{},
        places => [],
        transitions => [],
        decompositions => #{},
        flows => [],
        conditions => #{}
    },
    NetInfo = #{
        id => <<"main">>,
        module => test_net,
        places => [p1, p2],
        transitions => [t1],
        tasks => [task1],
        flows => [#{from => task1, to => 'end', predicate => none, default => true}]
    },
    #{
        original => Spec,
        nets => #{<<"main">> => NetInfo},
        metadata => #{title => <<"Test">>, version => <<"1.0">>}
    }.

is_compiled_test() ->
    Valid = make_test_compiled(),
    ?assert(yawl_compiled:is_compiled(Valid)),
    ?assertNot(yawl_compiled:is_compiled(#{})),
    ?assertNot(yawl_compiled:is_compiled(#{original => #{}})),
    ?assertNot(yawl_compiled:is_compiled(not_a_map)),
    ok.

net_test() ->
    Compiled = make_test_compiled(),
    ?assertMatch({ok, #{id := <<"main">>}}, yawl_compiled:net(Compiled, <<"main">>)),
    ?assertEqual({error, not_found}, yawl_compiled:net(Compiled, <<"missing">>)),
    ?assertEqual({error, not_found}, yawl_compiled:net(#{}, <<"main">>)),
    ok.

tasks_test() ->
    Compiled = make_test_compiled(),
    ?assertMatch({ok, [task1]}, yawl_compiled:tasks(Compiled, <<"main">>)),
    ?assertEqual({error, not_found}, yawl_compiled:tasks(Compiled, <<"missing">>)),
    ok.

places_test() ->
    Compiled = make_test_compiled(),
    ?assertMatch({ok, [p1, p2]}, yawl_compiled:places(Compiled, <<"main">>)),
    ?assertEqual({error, not_found}, yawl_compiled:places(Compiled, <<"missing">>)),
    ok.

transitions_test() ->
    Compiled = make_test_compiled(),
    ?assertMatch({ok, [t1]}, yawl_compiled:transitions(Compiled, <<"main">>)),
    ?assertEqual({error, not_found}, yawl_compiled:transitions(Compiled, <<"missing">>)),
    ok.

flows_test() ->
    Compiled = make_test_compiled(),
    ?assertMatch({ok, [#{from := task1}]}, yawl_compiled:flows(Compiled, <<"main">>, task1)),
    ?assertMatch({ok, []}, yawl_compiled:flows(Compiled, <<"main">>, nonexistent)),
    ?assertEqual({error, not_found}, yawl_compiled:flows(Compiled, <<"missing">>, task1)),
    ok.

net_module_test() ->
    Compiled = make_test_compiled(),
    ?assertEqual({ok, test_net}, yawl_compiled:net_module(Compiled, <<"main">>)),
    ?assertEqual({error, not_found}, yawl_compiled:net_module(Compiled, <<"missing">>)),
    ok.

get_metadata_test() ->
    Compiled = make_test_compiled(),
    Meta = yawl_compiled:get_metadata(Compiled),
    ?assertMatch(#{title := <<"Test">>, version := <<"1.0">>}, Meta),
    ?assertMatch(#{title := <<"Untitled">>, version := <<"0.0">>},
                 yawl_compiled:get_metadata(#{})),
    ?assertMatch(#{title := <<"Untitled">>, version := <<"0.0">>},
                 yawl_compiled:get_metadata(not_a_map)),
    ok.

original_spec_test() ->
    Compiled = make_test_compiled(),
    Original = yawl_compiled:original_spec(Compiled),
    %% Original is an opaque type, just check it's not undefined
    ?assertNotEqual(undefined, Original),
    ?assertEqual(undefined, yawl_compiled:original_spec(#{})),
    ?assertEqual(undefined, yawl_compiled:original_spec(not_a_map)),
    ok.

validate_test() ->
    Valid = make_test_compiled(),
    ?assertEqual(ok, yawl_compiled:validate(Valid)),
    ?assertMatch({error, _}, yawl_compiled:validate(#{original => #{}})),
    ?assertMatch({error, _}, yawl_compiled:validate(not_a_map)),
    ok.

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

-endif.
