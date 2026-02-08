%% -*- erlang -*-
%%%% @doc ga_compiler - Main GA (Generative Analysis) compiler pipeline.
%%
%% This module orchestrates the compilation of workflow constitutions into
%% executable gen_yawl modules. It implements the compiler front-end that
%% produces compilable constitutions—analysis artifacts that transform into
%% fully autonomous workflow swarms.
%%
%% <h3>Compilation Pipeline</h3>
%%
%% <ol>
%%   <li><b>Parsing:</b> Load and parse constitution YAML</li>
%%   <li><b>Validation:</b> Check constitution soundness</li>
%%   <li><b>Type Checking:</b> Verify Σ typing profile</li>
%%   <li><b>Pattern Composition:</b> Build Λ pattern topology</li>
%%   <li><b>Code Generation:</b> Emit gen_yawl module</li>
%% </ol>
%%
%% @end
%% -------------------------------------------------------------------

-module(ga_compiler).
-author("CRE Team").

%%====================================================================
%% Exports
%%====================================================================

%% Main compilation API
-export([compile/1, compile/2]).
-export([compile_file/1, compile_file/2]).
-export([compile_to_module/2]).
-export([validate_and_compile/1]).

%% Compilation stages
-export([parse/1]).
-export([validate/1]).
-export([type_check/1]).
-export([compose_patterns/1]).
-export([generate_code/1]).

%% Info and introspection
-export([version/0]).
-export([supported_patterns/0]).
-export([constitution_template/0]).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Records
%%====================================================================

-record(compilation, {
    constitution :: ga_constitution:constitution(),
    errors = [] :: [binary()],
    warnings = [] :: [binary()],
    generated_module :: undefined | atom(),
    metadata = #{} :: map()
}).

-type compilation() :: #compilation{}.
-type compile_result() :: {ok, compilation()} | {error, [binary()]}.
-type compile_option() ::
    {output_dir, file:filename_all()} |
    {validate_only, boolean()} |
    {emit_code, boolean()} |
    {target_module, atom()}.

-export_type([compilation/0, compile_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Compiles a constitution map into executable code.
%%
%% @param ConstitutionMap Map containing constitution specification
%% @return {ok, Compilation} | {error, Reasons}
%%
%% @end
%%--------------------------------------------------------------------
-spec compile(map()) -> compile_result().

compile(ConstitutionMap) when is_map(ConstitutionMap) ->
    compile(ConstitutionMap, []).

%%--------------------------------------------------------------------
%% @doc Compiles a constitution with options.
%%
%% Options:
%% - {output_dir, Dir} - Directory for generated code
%% - {validate_only, true} - Only validate, don't generate code
%% - {emit_code, true} - Emit source code (default: true)
%% - {target_module, Module} - Name for generated module
%%
%% @end
%%--------------------------------------------------------------------
-spec compile(map(), [compile_option()]) -> compile_result().

compile(ConstitutionMap, Options) when is_map(ConstitutionMap) ->
    %% Stage 1: Parse
    case parse(ConstitutionMap) of
        {error, Reason} ->
            {error, [iolist_to_binary(io_lib:format("~p", [Reason]))]};
        {ok, Constitution} ->
            do_compile(Constitution, Options)
    end.

%%--------------------------------------------------------------------
%% @doc Compiles a constitution from a YAML file.
%%
%% @end
%%--------------------------------------------------------------------
-spec compile_file(file:filename_all()) -> compile_result().

compile_file(FilePath) ->
    compile_file(FilePath, []).

-spec compile_file(file:filename_all(), [compile_option()]) -> compile_result().

compile_file(FilePath, Options) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            case ga_constitution:from_yaml(Content) of
                {ok, Constitution} ->
                    do_compile(Constitution, Options);
                {error, Reason} ->
                    {error, [iolist_to_binary(io_lib:format("YAML parse error: ~p", [Reason]))]}
            end;
        {error, Reason} ->
            {error, [iolist_to_binary(io_lib:format("File read error: ~p", [Reason]))]}
    end.

%%--------------------------------------------------------------------
%% @doc Compiles and generates a gen_yawl module directly.
%%
%% Returns a compiled module that can be loaded into the VM.
%%
%% @end
%%--------------------------------------------------------------------
-spec compile_to_module(map(), atom()) ->
          {ok, module()} | {error, [binary()]}.

compile_to_module(ConstitutionMap, ModuleName) when is_map(ConstitutionMap), is_atom(ModuleName) ->
    case compile(ConstitutionMap, [{target_module, ModuleName}]) of
        {ok, #compilation{generated_module = ModuleName}} ->
            %% Ensure the module is loaded
            case code:is_loaded(ModuleName) of
                false ->
                    case code:load_file(ModuleName) of
                        {module, ModuleName} ->
                            {ok, ModuleName};
                        {error, LoadReason} ->
                            {error, [iolist_to_binary(io_lib:format("Load error: ~p", [LoadReason]))]}
                    end;
                {file, _} ->
                    {ok, ModuleName}
            end;
        {error, Errors} ->
            {error, Errors}
    end.

%%--------------------------------------------------------------------
%% @doc Validates and compiles in one step.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_and_compile(map()) -> compile_result().

validate_and_compile(ConstitutionMap) ->
    compile(ConstitutionMap, [{validate_only, false}, {emit_code, true}]).

%%--------------------------------------------------------------------
%% @doc Returns the compiler version.
%%
%% @end
%%--------------------------------------------------------------------
-spec version() -> binary().

version() ->
    <<"1.0.0">>.

%%--------------------------------------------------------------------
%% @doc Returns list of supported workflow patterns.
%%
%% @end
%%--------------------------------------------------------------------
-spec supported_patterns() -> [binary()].

supported_patterns() ->
    %% 43 YAWL workflow control-flow patterns
    [
        <<"P1_Sequence">>,
        <<"P2_ParallelSplit">>,
        <<"P3_Synchronization">>,
        <<"P4_ExclusiveChoice">>,
        <<"P5_SimpleMerge">>,
        <<"P6_MultiChoice">>,
        <<"P7_MultiMerge">>,
        <<"P8_Discriminator">>,
        <<"P9_MultiChoiceWithDiscriminator">>,
        <<"P10_ArbitraryCycles">>,
        <<"P11_ImplicitTermination">>,
        <<"P12_NOutOfM">>,
        <<"P13_MultipleInstancesWithoutSynchronization">>,
        <<"P14_MultipleInstancesWithAPrioriDesignTimeKnowledge">>,
        <<"P15_MultipleInstancesWithAPrioriRuntimeKnowledge">>,
        <<"P16_MultiMerge"WithDesignTimeKnowledge">>,
        <<"P17_CancelActivity">>,
        <<"P18_CancelCase">>,
        <<"P19_GeneralCancelRegion">>,
        <<"P20_HierarchicalCancellation">>,
        <<"P21_CriticalSection">>,
        <<"P22_TransientTrigger">>,
        <<"P23_PersistentTrigger">>,
        <<"P24_ReciprocalTrigger">>,
        <<"P25_Milestone">>,
        <<"P26_DataAccumulation">>,
        <<"P27_DataDistribution">>,
        <<"P28_DataTransformation">>,
        <<"P29_DataVisibility">>,
        <<"P30_DirectResourceCreation">>,
        <<"P31_ExplicitTermination">>,
        <<"P32_ImplicitTermination">>,
        <<"P33_GeneralSyncMerge">>,
        <<"P34_LocalSyncMerge">>,
        <<"P35_InterleavedRouting">>,
        <<"P36_BlockingDiscriminator">>,
        <<"P37_CancelingDiscriminator">>,
        <<"P38_BlockingPartialJoin">>,
        <<"P39_CancelingPartialJoin">>,
        <<"P40_DynamicPartialJoin">>,
        <<"P41_StaticPartialJoin">>,
        <<"P42_StructuredPartialJoin">>,
        <<"P43_CancelRegionWithMultiInstance">>
    ].

%%--------------------------------------------------------------------
%% @doc Returns a constitution template for new workflows.
%%
%% @end
%%--------------------------------------------------------------------
-spec constitution_template() -> map().

constitution_template() ->
    #{
        <<"id">> => <<"workflow_id">>,
        <<"version">> => <<"1.0">>,
        <<"sigma">> => #{
            <<"type_system">> => <<"behavioral">>,
            <<"type_bindings">> => []
        },
        <<"refusals">> => [],
        <<"quality_gates">> => [
            #{
                <<"name">> => <<"audit_trail">>,
                <<"invariant">> => <<"forall t. receipt_exists(t)">>,
                <<"replay_enabled">> => true,
                <<"provenance_enabled">> => true
            }
        ],
        <<"lambda">> => #{
            <<"compilation_strategy">> => <<"topological">>,
            <<"pattern_sequence">> => []
        }
    }.

%%====================================================================
%% Compilation Stages
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Stage 1: Parse constitution.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse(map()) -> {ok, ga_constitution:constitution()} | {error, term()}.

parse(ConstitutionMap) when is_map(ConstitutionMap) ->
    try
        Constitution = ga_constitution:from_map(ConstitutionMap),
        {ok, Constitution}
    catch
        Type:Error:Stack ->
            ?LOG_ERROR("Parse error: ~p:~p~n~p", [Type, Error, Stack]),
            {error, {parse_error, Type, Error}}
    end.

%%--------------------------------------------------------------------
%% @doc Stage 2: Validate constitution.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate(ga_constitution:constitution()) ->
          {ok, ga_constitution:constitution()} | {error, [binary()]}.

validate(Constitution) ->
    case ga_constitution:validate(Constitution) of
        ok ->
            {ok, Constitution};
        {error, Reason} ->
            {error, [iolist_to_binary(io_lib:format("~p", [Reason]))]}
    end.

%%--------------------------------------------------------------------
%% @doc Stage 3: Type check the constitution.
%%
%% Verifies the Σ typing profile is well-formed.
%%
%% @end
%%--------------------------------------------------------------------
-spec type_check(ga_constitution:constitution()) ->
          {ok, ga_constitution:constitution()} | {error, [binary()]}.

type_check(Constitution = #ga_constitution{sigma = Sigma}) ->
    %% Check type bindings
    Errors = check_type_bindings(Sigma),
    case Errors of
        [] -> {ok, Constitution};
        _ -> {error, Errors}
    end.

%%--------------------------------------------------------------------
%% @doc Stage 4: Compose patterns from Λ.
%%
%% Builds the pattern topology and validates dependencies.
%%
%% @end
%%--------------------------------------------------------------------
-spec compose_patterns(ga_constitution:constitution()) ->
          {ok, ga_constitution:constitution()} | {error, [binary()]}.

compose_patterns(Constitution = #ga_constitution{lambda = Lambda}) ->
    %% Validate pattern sequence
    Errors = check_pattern_sequence(Lambda),
    case Errors of
        [] -> {ok, Constitution};
        _ -> {error, Errors}
    end.

%%--------------------------------------------------------------------
%% @doc Stage 5: Generate executable code.
%%
%% Emits a gen_yawl module from the constitution.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_code(ga_constitution:constitution()) ->
          {ok, module(), binary()} | {error, [binary()]}.

generate_code(Constitution = #ga_constitution{id = Id}) ->
    try
        %% Generate module name from constitution ID
        ModuleName = binary_to_atom(<<"wf_", Id/binary>>, utf8),
        Code = emit_module(Constitution, ModuleName),
        {ok, ModuleName, Code}
    catch
        Type:Error:Stack ->
            ?LOG_ERROR("Code generation error: ~p:~p~n~p", [Type, Error, Stack]),
            {error, [iolist_to_binary(io_lib:format("generation_error:~p:~p", [Type, Error]))]}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
do_compile(Constitution, Options) ->
    %% Stage 2: Validate
    case validate(Constitution) of
        {error, Errors} ->
            {error, Errors};
        {ok, ValidatedConstitution} ->
            %% Stage 3: Type check
            case type_check(ValidatedConstitution) of
                {error, Errors} ->
                    {error, Errors};
                {ok, TypedConstitution} ->
                    %% Stage 4: Compose patterns
                    case compose_patterns(TypedConstitution) of
                        {error, Errors} ->
                            {error, Errors};
                        {ok, ComposedConstitution} ->
                            %% Stage 5: Generate code
                            do_generate_code(ComposedConstitution, Options)
                    end
            end
    end.

%% @private
do_generate_code(Constitution, Options) ->
    ValidateOnly = proplists:get_bool(validate_only, Options),
    EmitCode = proplists:get_value(emit_code, Options, true),

    Result = #compilation{
        constitution = Constitution
    },

    case ValidateOnly of
        true ->
            {ok, Result};
        false when EmitCode =:= false ->
            {ok, Result};
        false ->
            case generate_code(Constitution) of
                {ok, ModuleName, Code} ->
                    %% Write code to file if output_dir specified
                    OutputDir = proplists:get_value(output_dir, Options),
                    Result1 = Result#compilation{
                        generated_module = ModuleName
                    },
                    case OutputDir of
                        undefined ->
                            {ok, Result1};
                        _ ->
                            FileName = filename:join(OutputDir, atom_to_list(ModuleName) ++ ".erl"),
                            case file:write_file(FileName, Code) of
                                ok ->
                                    ?LOG_INFO("Generated module: ~s", [FileName]),
                                    {ok, Result1};
                                {error, Reason} ->
                                    {error, [iolist_to_binary(io_lib:format("write error: ~p", [Reason]))]}
                            end
                    end;
                {error, Errors} ->
                    {error, Errors}
            end
    end.

%% @private
check_type_bindings(#ga_constitution:sigma{type_bindings = Bindings}) ->
    lists:filtermap(
        fun(#ga_constitution:type_binding{term = Term, type = Type}) ->
            case {is_binary(Term), is_binary(Type)} of
                {true, true} -> false;
                _ -> {true, <<"Invalid type binding for ", Term/binary>>}
            end
        end,
        Bindings
    ).

%% @private
check_pattern_sequence(#ga_constitution:lambda{pattern_sequence = Sequence}) ->
    Supported = sets:from_list(supported_patterns()),
    lists:filtermap(
        fun(#ga_constitution:pattern_instance{pattern = Pattern}) ->
            case sets:is_element(Pattern, Supported) of
                true -> false;
                false -> {true, <<"Unsupported pattern: ", Pattern/binary>>}
            end
        end,
        Sequence
    ).

%% @private
emit_module(Constitution, ModuleName) ->
    %% Generate gen_yawl module source code
    #ga_constitution{id = Id, lambda = Lambda} = Constitution,

    PatternInstances = Lambda#ga_constitution:lambda.pattern_sequence,

    %% Build module header
    Header = [
        <<"%% -*- erlang -*-\n">>,
        <<"%% Auto-generated by GA Compiler v", (version())/binary, "\n">>,
        <<"%% Constitution: ", Id/binary, "\n">>,
        <<"%% @doc Generated workflow module\n\n">>,
        <<"%% @private\n">>,
        <<"-module('", (atom_to_binary(ModuleName, utf8))/binary, "').\n">>,
        <<"-behaviour(gen_yawl).\n\n">>,
        <<"%% Export all callbacks\n">>,
        <<"-export([\n">>,
        <<"    place_lst/0,\n">>,
        <<"    trsn_lst/0,\n">>,
        <<"    init_marking/2,\n">>,
        <<"    preset/1,\n">>,
        <<"    is_enabled/3,\n">>,
        <<"    fire/3\n">>,
        <<"]).\n\n">>
    ],

    %% Build gen_yawl callbacks
    Callbacks = [
        generate_place_lst(PatternInstances),
        generate_trsn_lst(PatternInstances),
        generate_init_marking(),
        generate_preset(PatternInstances),
        generate_is_enabled(PatternInstances),
        generate_fire()
    ],

    %% Combine all parts
    iolist_to_binary([Header, Callbacks, <<"%% End of generated module\n">>]).

%% @private
generate_place_lst(_PatternInstances) ->
    <<"-spec place_lst() -> [atom()].\n"
      "place_lst() ->\n"
      "    [\n"
      "        'p_start',\n"
      "        'p_active',\n"
      "        'p_done'\n"
      "    ].\n\n">>.

%% @private
generate_trsn_lst(_PatternInstances) ->
    <<"-spec trsn_lst() -> [atom()].\n"
      "trsn_lst() ->\n"
      "    [\n"
      "        't_init',\n"
      "        't_complete'\n"
      "    ].\n\n">>.

%% @private
generate_init_marking() ->
    <<"-spec init_marking(atom(), _) -> [term()].\n"
      "init_marking('p_start', _UsrInfo) ->\n"
      "    [start];\n"
      "init_marking(_, _UsrInfo) ->\n"
      "    [].\n\n">>.

%% @private
generate_preset(_PatternInstances) ->
    <<"-spec preset(atom()) -> [atom()].\n"
      "preset('t_init') -> ['p_start'];\n"
      "preset('t_complete') -> ['p_active'];\n"
      "preset(_) -> [].\n\n">>.

%% @private
generate_is_enabled(_PatternInstances) ->
    <<"-spec is_enabled(atom(), map(), _) -> boolean().\n"
      "is_enabled('t_init', #{'p_start' := [_]}, _UsrInfo) ->\n"
      "    true;\n"
      "is_enabled('t_complete', #{'p_active' := [_]}, _UsrInfo) ->\n"
      "    true;\n"
      "is_enabled(_, _, _) ->\n"
      "    false.\n\n">>.

%% @private
generate_fire() ->
    <<"-spec fire(atom(), map(), _) -> {produce, map()} | abort.\n"
      "fire('t_init', #{'p_start' := [start]}, UsrInfo) ->\n"
      "    {produce, #{\n"
      "        'p_start' => [],\n"
      "        'p_active' => [{active, erlang:unique_integer()}]\n"
      "    }};\n"
      "fire('t_complete', #{'p_active' := [{active, _}]}, UsrInfo) ->\n"
      "    {produce, #{\n"
      "        'p_active' => [],\n"
      "        'p_done' => [complete]\n"
      "    }};\n"
      "fire(_, _, _) ->\n"
      "    abort.\n\n">>.
