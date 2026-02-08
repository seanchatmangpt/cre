%% -*- erlang -*-
%%%% @doc YAWL to pnet_net compiler.
%%
%% **Joe Armstrong Design: Pure Helper Module (Stateless)**
%%
%% This module provides pure functional code generation. No state is maintained -
%% all compilation functions are stateless transformations from YAWL specs to
%% Erlang module source code.
%%
%% This module compiles YAWL 2.1/2.2 specifications into gen_pnet compatible
%% Petri net modules. Each YAWL decomposition becomes a standalone Erlang
%% module implementing the gen_pnet behavior.
%%
%% == Compilation Overview ==
%%
%% The compiler transforms YAWL workflow specifications into Petri net
%% structures by:
%%
%% 1. Extracting tasks, flows, and conditions from the YAWL spec
%% 2. Generating places for input conditions, output conditions, and tasks
%% 3. Generating transitions with proper presets (input places)
%% 4. Creating mode enumeration and fire functions for each transition
%% 5. Handling split/join types (AND, OR, XOR) for parallel execution
%%
%% == Generated Module Structure ==
%%
%% Each compiled module implements the gen_pnet callbacks:
%%
%% ```
%% -module(yawl_NetId).
%% -behaviour(gen_pnet).
%%
%% place_lst() -> [input, output, task1, task2, ...].
%% trsn_lst() -> [t_task1, t_task2, ...].
%% init_marking(Place, UsrInfo) -> ... % initial tokens
%% preset(Transition) -> ... % input places
%% is_enabled(Transition, Mode, UsrInfo) -> ...
%% fire(Transition, Mode, UsrInfo) -> ...
%% init(NetArg) -> UsrInfo.
%% handle_call/3, handle_cast/2, handle_info/2, ...
%% ```
%%
%% == Options ==
%%
%% The compiler accepts an options map with the following keys:
%%
%% - `seed` - Non-negative integer for deterministic random number generation
%% - `module_prefix` - Binary prefix for generated module names (default: <<"yawl_">>)
%% - `output_dir` - Directory path for writing compiled modules
%% - `include_source` - Include original YAWL source in generated module docs
%% - `gen_observer` - Generate observer callbacks for state monitoring
%%
%% == Examples ==
%%
%% Basic compilation:
%%
%% ```erlang
%% > {ok, Spec} = wf_spec:from_xml_file("my_workflow.yawl"),
%% > {ok, Compiled} = yawl_compile:compile(Spec, #{}).
%% '''
%%
%% Compile to file:
%%
%% ```erlang
%% > {ok, Files} = yawl_compile:compile_to_file(Spec, #{}, "src/compiled").
%% '''
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_compile).
-moduledoc """
YAWL to pnet_net compiler.

Compiles YAWL 2.1/2.2 specifications to gen_pnet compatible Petri net modules.
Each YAWL decomposition becomes a gen_pnet behavior module.

## Examples

```erlang
%% Parse YAWL XML and compile to gen_pnet modules
> Xml = <<
.. "<specificationSet id='demo' version='2.2'>"
.. "<specification uri='demo'>"
.. "<decomposition id='main' isRootNet='true'>"
.. "<processControlElements>"
.. "<task id='t1'/>"
.. "</processControlElements>"
.. "</decomposition>"
.. "</specification>"
.. "</specificationSet>">>,
> {ok, Spec} = wf_spec:from_xml(Xml),
> {ok, Compiled} = yawl_compile:compile(Spec, #{}).
{ok, #{spec_id := <<"demo">>, modules := _, places := _, transitions := _, net_info := _}}
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% Main compilation API
-export([compile/2, compile_to_file/3]).

%% Code generation API
-export([generate_module/2, generate_places/1, generate_transitions/1,
         generate_place_lst/1, generate_trsn_lst/1]).

%% Internal helpers (exported for testing)
-export([sanitize_atom_name/1, place_atom/2, transition_atom/2]).
-export([build_flow_map/1, extract_net_tasks/2]).
-export([generate_preset_clauses/2, generate_fire_clause/4, generate_fire_clauses/3]).

%%====================================================================
%% Includes
%%====================================================================

-include("../../include/gen_pnet.hrl").

%% Macros for YAWL split/join types (reserved words)
-define(XOR, 'xor').
-define(AND, 'and').
-define(OR, 'or').

%%====================================================================
%% Types
%%====================================================================

-type spec() :: wf_spec:yawl_spec().
-type task_id() :: wf_spec:task_id().
-type place() :: atom().
-type transition() :: atom().
-type net_id() :: binary().

-type compile_option() ::
    {seed, non_neg_integer()} |
    {module_prefix, binary()} |
    {output_dir, file:filename_all()} |
    {include_source, boolean()} |
    {gen_observer, boolean()}.

-type compile_options() :: #{
    seed := non_neg_integer(),
    module_prefix := binary(),
    output_dir => file:filename_all(),
    include_source => boolean(),
    gen_observer => boolean()
}.

-type compile_result() :: #{
    spec_id := binary(),
    modules := #{binary() => module()},
    places := #{binary() => [place()]},
    transitions := #{binary() => [transition()]},
    net_info := #{binary() => map()}
}.

-type net_info() :: #{
    id := binary(),
    tasks := [task_id()],
    input_condition := binary(),
    output_condition := binary(),
    flows := [{task_id(), task_id(), binary() | undefined}],
    split_types := #{task_id() => atom()},
    join_types := #{task_id() => atom()}
}.

-export_type([compile_option/0, compile_options/0, compile_result/0, net_info/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Compiles a YAWL specification to in-memory Erlang modules.
%%
%% Takes a parsed YAWL specification from `wf_spec:from_xml/1` and
%% generates Erlang module code for each decomposition net.
%%
%% Returns `{ok, Compiled}` where Compiled is a map containing:
%% - `spec_id` - The specification identifier
%% - `modules` - Map of net_id to module code binaries
%% - `places` - Map of net_id to place lists
%% - `transitions` - Map of net_id to transition lists
%% - `net_info` - Map of net_id to net structure info
%%
%% @end
%%--------------------------------------------------------------------
-spec compile(Spec :: spec(), Options :: map()) ->
    {ok, compile_result()} | {error, Reason :: term()}.

compile(Spec, Options) when is_tuple(Spec), element(1, Spec) =:= yawl_spec ->
    SpecId = wf_spec:id(Spec),
    DecompsList = wf_spec:all_decompositions(Spec),
    %% Convert list of {DecompId, IsRoot, Tasks} to map
    Decomps = lists:foldl(fun({DecompId, IsRoot, Tasks}, Acc) ->
        Acc#{DecompId => #{
            id => DecompId,
            is_root => IsRoot,
            tasks => Tasks
        }}
    end, #{}, DecompsList),
    try
        CompileOpts = normalize_options(Options),
        NetInfos = build_net_infos(SpecId, Decomps),

        Modules = maps:fold(fun(NetId, NetInfo, Acc) ->
            case generate_module(NetId, NetInfo, CompileOpts) of
                {ok, ModuleCode} ->
                    Acc#{NetId => ModuleCode};
                {error, Reason} ->
                    throw({module_generation_error, NetId, Reason})
            end
        end, #{}, NetInfos),

        {Places, Transitions} = extract_places_and_transitions(Modules, NetInfos),

        Compiled = #{
            spec_id => SpecId,
            modules => Modules,
            places => Places,
            transitions => Transitions,
            net_info => NetInfos
        },

        {ok, Compiled}
    catch
        throw:{compile_error, Reason} -> {error, Reason};
        _:Reason:Stack -> {error, {compile_exception, Reason, Stack}}
    end;
compile(Spec, Options) when is_tuple(Spec), element(1, Spec) =:= yawl_yaml_spec ->
    compile_yaml_spec(Spec, Options);
compile(_Spec, _Options) ->
    {error, invalid_spec}.

%% Compile YAML-based YAWL spec (new implementation)
compile_yaml_spec(Spec, Options) ->
    SpecId = wf_yaml_spec:id(Spec),
    Nets = wf_yaml_spec:nets(Spec),
    PatternInstances = wf_yaml_spec:pattern_instances(Spec),
    Context = #{spec => Spec},

    %% Build net infos with per-net pattern expansion
    NetInfos = build_yaml_net_infos(Spec, Nets, PatternInstances, Context),
    
    try
        CompileOpts = normalize_options(Options),
        
        Modules = maps:fold(fun(NetId, NetInfo, Acc) ->
            case generate_module(NetId, NetInfo, CompileOpts) of
                {ok, ModuleCode} ->
                    Acc#{NetId => ModuleCode};
                {error, Reason} ->
                    throw({module_generation_error, NetId, Reason})
            end
        end, #{}, NetInfos),
        
        {Places, Transitions} = extract_places_and_transitions(Modules, NetInfos),
        
        Compiled = #{
            spec_id => SpecId,
            modules => Modules,
            places => Places,
            transitions => Transitions,
            net_info => NetInfos
        },
        
        {ok, Compiled}
    catch
        throw:{compile_error, Reason} -> {error, Reason};
        _:Reason:Stack -> {error, {compile_exception, Reason, Stack}}
    end.

%% Build net infos from YAML spec with per-net pattern expansion.
build_yaml_net_infos(Spec, Nets, PatternInstances, Context) ->
    RootNet = wf_yaml_spec:root_net(Spec),
    RootNetNorm = case RootNet of
        B when is_binary(B) -> B;
        A when is_atom(A) -> atom_to_binary(A, utf8);
        _ -> <<"">>
    end,
    lists:foldl(fun(NetId, Acc) ->
        NetIdNorm = case NetId of
            N when is_binary(N) -> N;
            N when is_atom(N) -> atom_to_binary(N, utf8);
            _ -> <<"">>
        end,
        Tasks = wf_yaml_spec:tasks(Spec, NetId),
        Expanded = yawl_pattern_expander:expand_patterns_for_net(PatternInstances, NetId, Context),
        InputCond = wf_yaml_spec:net_input_condition(Spec, NetId),
        OutputCond = wf_yaml_spec:net_output_condition(Spec, NetId),
        Variables = wf_yaml_spec:variables(Spec, NetId),
        Regions = wf_yaml_spec:net_regions(Spec, NetId),
        ExpandedPlaces = maps:get(places, Expanded, []),
        ExpandedPreset = maps:get(preset, Expanded, #{}),
        ExpandedPostset = maps:get(postset, Expanded, #{}),
        %% Ensure all preset/postset places are in place_lst (prevents enum_mode badmatch)
        PresetPlaces = lists:usort(lists:flatten(maps:values(ExpandedPreset))),
        PostsetPlaces = lists:usort(lists:flatten(maps:values(ExpandedPostset))),
        AllReferencedPlaces = lists:usort(PresetPlaces ++ PostsetPlaces),
        Places = case ExpandedPlaces of
            [_ | _] -> lists:usort(ExpandedPlaces ++ AllReferencedPlaces);
            [] -> []
        end,
        NetInfo = #{
            id => NetId,
            spec_id => wf_yaml_spec:id(Spec),
            is_root => (NetIdNorm =:= RootNetNorm),
            tasks => Tasks,
            places => Places,
            transitions => maps:get(transitions, Expanded, []),
            preset => ExpandedPreset,
            postset => ExpandedPostset,
            flows => maps:get(flows, Expanded, []),
            split_types => #{},
            join_types => #{},
            input_condition => InputCond,
            output_condition => OutputCond,
            variables => Variables,
            regions => build_region_places_map(Regions, ExpandedPlaces, PatternInstances, NetId, Spec)
        },
        Acc#{NetId => NetInfo}
    end, #{}, Nets).


%%--------------------------------------------------------------------
%% @doc Compiles a YAWL specification and writes modules to files.
%%
%% Same as `compile/2` but writes each generated module to a file
%% in the specified output directory. Files are named `yawl_<NetId>.erl`.
%%
%% Returns `{ok, Files}` with a list of generated file paths,
%% or `{error, Reason}` on failure.
%%
%% @end
%%--------------------------------------------------------------------
-spec compile_to_file(Spec :: spec(), Options :: map(),
                      OutputDir :: file:filename_all()) ->
    {ok, [file:filename_all()]} | {error, Reason :: term()}.

compile_to_file(Spec, Options, OutputDir) ->
    case compile(Spec, Options#{output_dir => OutputDir}) of
        {ok, #{modules := Modules, spec_id := SpecId}} ->
            write_modules_to_files(Modules, OutputDir, SpecId);
        {error, Reason} ->
            {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc Generates a complete Erlang module for a YAWL decomposition net.
%%
%% Returns `{ok, ModuleCode}` where ModuleCode is a binary containing
%% the complete Erlang source code for a gen_pnet compatible module.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_module(NetId :: net_id(), NetInfo :: net_info()) ->
    {ok, binary()} | {error, Reason :: term()}.

generate_module(NetId, NetInfo) ->
    generate_module(NetId, NetInfo, normalize_options(#{})).


%%--------------------------------------------------------------------
%% @doc Generates the list of places for a YAWL net.
%%
%% For YAML specs with pattern expansion, uses Expanded.places when present.
%% Otherwise uses input condition, output condition, and one per task.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_places(NetInfo :: net_info()) -> [place()].

generate_places(#{places := ExpandedPlaces}) when is_list(ExpandedPlaces), ExpandedPlaces =/= [] ->
    lists:usort(ExpandedPlaces);
generate_places(#{input_condition := InputCond,
                  output_condition := OutputCond,
                  tasks := Tasks}) ->
    InputPlace = sanitize_atom_name(InputCond),
    OutputPlace = sanitize_atom_name(OutputCond),
    TaskPlaces = [sanitize_atom_name(TaskId) || TaskId <- Tasks],
    [InputPlace, OutputPlace | TaskPlaces].


%%--------------------------------------------------------------------
%% @doc Generates the list of transitions for a YAWL net.
%%
%% For YAML specs with pattern expansion, uses Expanded.transitions when present.
%% Otherwise generates one transition per task.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_transitions(NetInfo :: net_info()) -> [transition()].

generate_transitions(#{transitions := ExpandedTrans}) when is_list(ExpandedTrans), ExpandedTrans =/= [] ->
    ExpandedTrans;
generate_transitions(#{tasks := Tasks}) ->
    [transition_atom(TaskId, <<"t_">>) || TaskId <- Tasks].

%%====================================================================
%% Internal Functions - Options
%%====================================================================

%% @private Build region id -> places map for cancellation. Regions from YAML have id and
%% cancel_region; places are inferred from P25 CancelRegion pattern instances when available.
build_region_places_map(Regions, ExpandedPlaces, PatternInstances, NetId, Spec) when is_list(Regions) ->
    NetIdNorm = case NetId of
        N when is_binary(N) -> N;
        N when is_atom(N) -> atom_to_binary(N, utf8);
        _ -> <<>>
    end,
    P25ForNet = [I || I <- PatternInstances,
        net_matches(I, NetIdNorm),
        pi_pattern_is(I, <<"P25_CancelRegion">>)],
    lists:foldl(fun(R, Acc) ->
        Id = maps:get(<<"id">>, R, maps:get(id, R, undefined)),
        case Id of
            undefined -> Acc;
            B when is_binary(B) ->
                Places = places_for_region(B, P25ForNet, ExpandedPlaces, Spec),
                Acc#{B => Places};
            A when is_atom(A) ->
                Bin = atom_to_binary(A, utf8),
                Places = places_for_region(Bin, P25ForNet, ExpandedPlaces, Spec),
                Acc#{A => Places}
        end
    end, #{}, Regions);
build_region_places_map(_, _, _, _, _) -> #{}.

pi_pattern_is(I, P) when is_map(I) ->
    PN = maps:get(pattern, I, maps:get(<<"pattern">>, I, undefined)),
    PN =:= P orelse (is_atom(PN) andalso atom_to_binary(PN, utf8) =:= P).

net_matches(I, NetId) when is_binary(NetId) ->
    N = maps:get(net, I, maps:get(<<"net">>, I, undefined)),
    case N of
        B when is_binary(B) -> B =:= NetId;
        A when is_atom(A) -> atom_to_binary(A, utf8) =:= NetId;
        _ -> false
    end.

places_for_region(RegionId, P25Instances, ExpandedPlaces, Spec) ->
    %% Find P25 instance with region param matching RegionId
    Match = lists:search(fun(I) ->
        R = maps:get(region, I, maps:get(<<"region">>, I, undefined)),
        case R of
            B when is_binary(B) -> B =:= RegionId;
            A when is_atom(A) -> atom_to_binary(A, utf8) =:= RegionId;
            _ -> false
        end
    end, P25Instances),
    case Match of
        {value, Inst} ->
            %% Expand this instance to get its places (with mapping applied)
            Context = #{spec => Spec},
            Expanded = yawl_pattern_expander:expand_pattern(Inst, Context),
            P = maps:get(places, Expanded, []),
            %% Intersect with ExpandedPlaces so we only include places that exist in the net
            [Pl || Pl <- P, lists:member(Pl, ExpandedPlaces)];
        false ->
            %% No P25 for this region; use cancel_region pattern default places if in net
            Defaults = [p_start, p_region_active, p_cancel_event, p_region_cancelled, p_end],
            [Pl || Pl <- Defaults, lists:member(Pl, ExpandedPlaces)]
    end.

%% @private
normalize_options(Options) when is_map(Options) ->
    Defaults = #{
        seed => 0,
        module_prefix => <<"yawl_">>,
        include_source => false,
        gen_observer => false
    },
    maps:merge(Defaults, Options).

%%====================================================================
%% Internal Functions - Net Information Building
%%====================================================================

%% @private
build_net_infos(SpecId, Decomps) ->
    maps:fold(fun(DecompId, DecompInfo, Acc) ->
        Id = maps:get(id, DecompInfo, DecompId),
        IsRoot = maps:get(is_root, DecompInfo, false),
        Tasks = maps:get(tasks, DecompInfo, []),
        NetInfo = #{
            id => Id,
            spec_id => SpecId,
            is_root => IsRoot,
            tasks => Tasks,
            input_condition => <<Id/binary, "_input">>,
            output_condition => <<Id/binary, "_output">>,
            flows => [],
            split_types => #{},
            join_types => #{}
        },
        Acc#{DecompId => NetInfo}
    end, #{}, Decomps).

%%====================================================================
%% Internal Functions - Module Generation
%%====================================================================

%% @private
generate_module(NetId, NetInfo, Options) ->
    ModuleName = module_name(NetId, Options),

    Places = generate_places(NetInfo),
    Transitions = generate_transitions(NetInfo),

    ModuleCode = iolist_to_binary([
        <<"%% -*- erlang -*-\n"
           "%%\n"
           "%% Auto-generated YAWL workflow module\n"
           "%% Generated by: yawl_compile\n"
           "%% Net ID: ">>, escape_binary(NetId), <<"\n"
           "%% DO NOT EDIT - This file is auto-generated\n"
           "%%\n"
           "\n"
           "-module(">>, atom_to_binary(ModuleName, utf8), <<").\n"
           "-behaviour(gen_pnet).\n"
           "\n"
           "-moduledoc \"\"\"\n"
           "Auto-generated YAWL workflow net module.\n"
           "\n"
           "This module implements the '">>, escape_binary(NetId),
           <<"' decomposition net\n"
           "from the YAWL specification.\n"
           "\"\"\".\n"
           "\n"
           "%%====================================================================\n"
           "%% Exports\n"
           "%%====================================================================\n"
           "\n"
           "%% gen_pnet structure callbacks\n"
           "-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,\n"
           "         is_enabled/3, fire/3]).\n"
           "\n"
           "%% gen_pnet interface callbacks\n"
           "-export([init/1, handle_call/3, handle_cast/2, handle_info/2,\n"
           "         code_change/3, terminate/2, trigger/3]).\n"
           "\n"
           "%%====================================================================\n"
           "%% Includes\n"
           "%%====================================================================\n"
           "\n"
           "-include_lib(\"gen_pnet/include/gen_pnet.hrl\").\n"
           "\n"
           "%%====================================================================\n"
           "%% gen_pnet Structure Callbacks\n"
           "%%====================================================================\n"
           "\n">>,
        generate_place_lst(Places),
        <<"\n">>,
        generate_trsn_lst(Transitions),
        <<"\n">>,
        generate_init_marking(NetInfo),
        <<"\n">>,
        generate_preset(NetInfo),
        <<"\n">>,
        generate_is_enabled(NetInfo),
        <<"\n">>,
        generate_fire(NetInfo),
        <<"\n"
           "%%====================================================================\n"
           "%% gen_pnet Interface Callbacks\n"
           "%%====================================================================\n"
           "\n"
           "%% @private\n"
           "-spec init(term()) -> term().\n"
           "init(NetArg) ->\n"
           "    ">>, generate_init_usr_info(NetInfo), <<".\n"
           "\n"
           "%% @private\n"
           "-spec handle_call(term(), {pid(), term()}, #net_state{}) ->\n"
           "    {reply, term()} | {reply, term(), #{atom() => [_]}} | noreply.\n"
           "handle_call(_Request, _From, _NetState) ->\n"
           "    {reply, {error, unknown_request}}.\n"
           "\n"
           "%% @private\n"
           "-spec handle_cast(term(), #net_state{}) -> noreply.\n"
           "handle_cast(_Request, _NetState) ->\n"
           "    noreply.\n"
           "\n"
           "%% @private\n"
           "-spec handle_info(term(), #net_state{}) -> noreply.\n"
           "handle_info(_Info, _NetState) ->\n"
           "    noreply.\n"
           "\n"
           "%% @private\n"
           "-spec code_change(term(), #net_state{}, term()) -> {ok, #net_state{}}.\n"
           "code_change(_OldVsn, NetState, _Extra) ->\n"
           "    {ok, NetState}.\n"
           "\n"
           "%% @private\n"
           "-spec terminate(term(), #net_state{}) -> ok.\n"
           "terminate(_Reason, _NetState) ->\n"
           "    ok.\n"
           "\n"
           "%% @private\n"
           "-spec trigger(atom(), term(), #net_state{}) -> pass | drop.\n"
           "trigger(_Place, _Token, _NetState) ->\n"
           "    pass.\n">>
    ]),

    {ok, ModuleCode}.

%% @private
%% Generate init/1 body - merge variables from spec with initial_data from NetArg (F-010).
generate_init_usr_info(#{variables := Vars}) when is_list(Vars), Vars =/= [] ->
    %% Build #{name => initial} from variable definitions
    Entries = lists:map(fun(V) ->
        Name = var_name(V),
        Initial = var_initial(V),
        NameAtom = case Name of
            B when is_binary(B) -> binary_to_atom(B, utf8);
            A when is_atom(A) -> A;
            _ -> undefined
        end,
        case NameAtom of
            undefined -> [];
            _ -> [<<"    ", (atom_to_source(NameAtom))/binary, " => ", (value_to_source(Initial))/binary>>]
        end
    end, Vars),
    ValidEntries = [E || E <- Entries, E =/= []],
    case ValidEntries of
        [] ->
            <<"maps:get(initial_data, NetArg, #{})">>;
        _ ->
            DefaultMap = iolist_to_binary(lists:join(<<",\n           ">>, ValidEntries)),
            <<"DefaultVars = #{\n           ", DefaultMap/binary, "\n    },\n"
              "    case NetArg of\n"
              "        #{initial_data := Id} when is_map(Id) -> maps:merge(DefaultVars, Id);\n"
              "        _ -> DefaultVars\n"
              "    end">>
    end;
generate_init_usr_info(_) ->
    <<"maps:get(initial_data, NetArg, #{})">>.

var_name(#{<<"name">> := N}) -> N;
var_name(#{name := N}) -> N;
var_name(_) -> undefined.

var_initial(#{<<"initial">> := V}) -> V;
var_initial(#{initial := V}) -> V;
var_initial(_) -> undefined.

value_to_source(V) when is_boolean(V) -> atom_to_binary(V, utf8);
value_to_source(V) when is_integer(V) -> integer_to_binary(V);
value_to_source(V) when is_binary(V) -> iolist_to_binary([$", V, $"]);
value_to_source(V) when is_list(V) -> iolist_to_binary(io_lib:format("~p", [V]));
value_to_source(undefined) -> <<"undefined">>;
value_to_source(V) -> iolist_to_binary(io_lib:format("~p", [V])).

%% @private
module_name(NetId, #{module_prefix := Prefix}) ->
    PrefixBin = <<Prefix/binary, NetId/binary>>,
    list_to_atom(binary_to_list(PrefixBin)).

%% @private
ensure_atom(A) when is_atom(A) -> A;
ensure_atom(B) when is_binary(B) -> binary_to_atom(B, utf8);
ensure_atom(L) when is_list(L) -> list_to_atom(L);
ensure_atom(X) -> X.

%% @private
place_to_source(A) when is_atom(A) -> atom_to_source(A);
place_to_source(B) when is_binary(B) -> atom_to_source(binary_to_atom(B, utf8));
place_to_source(X) -> atom_to_source(X).

%% @private
%% Format atom for Erlang source: quotes when needed (uppercase/special chars).
atom_to_source(Atom) when is_atom(Atom) ->
    Str = atom_to_list(Atom),
    case needs_quoted_atom(Str) of
        true ->
            Escaped = lists:flatten([case C of $' -> "''"; _ -> [C] end || C <- Str]),
            iolist_to_binary([$', Escaped, $']);
        false ->
            list_to_binary(Str)
    end.

needs_quoted_atom([C | _]) when C >= $A, C =< $Z -> true;
needs_quoted_atom([C | _]) when C =:= $' -> true;
needs_quoted_atom([_ | Rest]) -> needs_quoted_atom(Rest);
needs_quoted_atom([]) -> false.

%% @private
escape_binary(Bin) ->
    escape_binary(Bin, <<>>).

escape_binary(<<>>, Acc) ->
    Acc;
escape_binary(<<$\\, Rest/binary>>, Acc) ->
    escape_binary(Rest, <<Acc/binary, $\\, $\\>>);
escape_binary(<<$", Rest/binary>>, Acc) ->
    escape_binary(Rest, <<Acc/binary, $\\, $">>);
escape_binary(<<$\n, Rest/binary>>, Acc) ->
    escape_binary(Rest, <<Acc/binary, $\\, $n>>);
escape_binary(<<C, Rest/binary>>, Acc) ->
    escape_binary(Rest, <<Acc/binary, C>>).

%%====================================================================
%% Internal Functions - Code Generation - place_lst/0
%%====================================================================

%% @private
generate_place_lst(Places) ->
    PlaceList = lists:map(fun(P) ->
        <<"  ", (atom_to_source(P))/binary>>
    end, Places),
    Joined = iolist_to_binary(lists:join(<<",\n">>, PlaceList)),
    iolist_to_binary([
        <<"%% @doc Returns the list of places in the net.\n"
           "%% @private\n"
           "-spec place_lst() -> [atom()].\n"
           "place_lst() ->\n"
           "    [\n">>, Joined, <<"\n    ].\n">>
    ]).

%%====================================================================
%% Internal Functions - Code Generation - trsn_lst/0
%%====================================================================

%% @private
generate_trsn_lst(Transitions) ->
    TrsnList = lists:map(fun(T) ->
        <<"  ", (atom_to_source(T))/binary>>
    end, Transitions),
    Joined = iolist_to_binary(lists:join(<<",\n">>, TrsnList)),
    iolist_to_binary([
        <<"%% @doc Returns the list of transitions in the net.\n"
           "%% @private\n"
           "-spec trsn_lst() -> [atom()].\n"
           "trsn_lst() ->\n"
           "    [\n">>, Joined, <<"\n    ].\n">>
    ]).

%%====================================================================
%% Internal Functions - Code Generation - init_marking/2
%%====================================================================

%% @private
generate_init_marking(#{places := [_ | _] = ExpandedPlaces, input_condition := InputCond}) ->
    %% Expanded net: input place is p_start or first in preset of t_split
    InputPlace = case lists:member(p_start, ExpandedPlaces) of
        true -> p_start;
        false -> sanitize_atom_name(InputCond)
    end,
    InitClauses = [
        <<"\n        ", (atom_to_source(InputPlace))/binary, " ->\n            [init]">>
    ] ++
    [ <<"\n        ", (atom_to_source(P))/binary, " ->\n            []">> || P <- ExpandedPlaces, P =/= InputPlace ] ++
    [ <<"\n        _Place ->\n            []">> ],
    JoinedClauses = iolist_to_binary(lists:join(<<";">>, InitClauses)),
    iolist_to_binary([
        <<"%% @doc Returns the initial marking for a given place.\n"
           "%% @private\n"
           "-spec init_marking(atom(), term()) -> [term()].\n"
           "init_marking(Place, UsrInfo) ->\n"
           "    case Place of">>, JoinedClauses, <<"\n    end.\n">>
    ]);
generate_init_marking(#{input_condition := InputCond, tasks := Tasks}) ->
    InputPlace = sanitize_atom_name(InputCond),
    InitClauses = [
        <<"\n        ", (atom_to_source(InputPlace))/binary, " ->\n            [init]">>
    ] ++
    [ generate_init_marking_clause(Task) || Task <- Tasks ] ++
    [ <<"\n        _Place ->\n            []">> ],
    JoinedClauses = iolist_to_binary(lists:join(<<";">>, InitClauses)),
    iolist_to_binary([
        <<"%% @doc Returns the initial marking for a given place.\n"
           "%% @private\n"
           "-spec init_marking(atom(), term()) -> [term()].\n"
           "init_marking(Place, UsrInfo) ->\n"
           "    case Place of">>, JoinedClauses, <<"\n    end.\n">>
    ]).

%% @private
generate_init_marking_clause(TaskId) ->
    Place = sanitize_atom_name(TaskId),
    <<
        "\n        ", (atom_to_source(Place))/binary, " ->\n            []"
    >>.

%%====================================================================
%% Internal Functions - Code Generation - preset/1
%%====================================================================

%% @private
generate_preset(#{preset := PresetMap}) when map_size(PresetMap) > 0 ->
    Clauses = maps:fold(fun(Trsn, PresetList, Acc) ->
        PlaceAtoms = [ensure_atom(P) || P <- PresetList],
        TrsnAtom = ensure_atom(Trsn),
        PlaceSources = [place_to_source(Pa) || Pa <- PlaceAtoms],
        PlacesStr = iolist_to_binary(lists:join(<<", ">>, PlaceSources)),
        [iolist_to_binary(["\n        ", place_to_source(TrsnAtom), " ->\n            [", PlacesStr, "]"]) | Acc]
    end, [], PresetMap),
    JoinedClauses = iolist_to_binary(lists:join(<<";">>, Clauses ++ [<<"\n        _Transition ->\n            []">>])),
    iolist_to_binary([
        <<"%% @doc Returns the preset (input places) for a transition.\n"
           "%% @private\n"
           "-spec preset(atom()) -> [atom()].\n"
           "preset(Transition) ->\n"
           "    case Transition of">>,
        JoinedClauses,
        <<"\n    end.\n">>
    ]);
generate_preset(#{input_condition := InputCond, tasks := Tasks}) ->
    InputPlace = sanitize_atom_name(InputCond),
    Clauses = [ generate_preset_clause(Task, InputPlace) || Task <- Tasks ] ++
              [ <<"\n        _Transition ->\n            []">> ],
    JoinedClauses = iolist_to_binary(lists:join(<<";">>, Clauses)),
    iolist_to_binary([
        <<"%% @doc Returns the preset (input places) for a transition.\n"
           "%% @private\n"
           "-spec preset(atom()) -> [atom()].\n"
           "preset(Transition) ->\n"
           "    case Transition of">>,
        JoinedClauses,
        <<"\n    end.\n">>
    ]).

%% @private
generate_preset_clause(TaskId, InputPlace) ->
    Transition = transition_atom(TaskId, <<"t_">>),
    TaskPlace = sanitize_atom_name(TaskId),
    <<
        "\n        ", (atom_to_source(Transition))/binary,
        " ->\n            [", (atom_to_source(InputPlace))/binary,
        ", ", (atom_to_source(TaskPlace))/binary, "]"
    >>.

%%====================================================================
%% Internal Functions - Code Generation - is_enabled/3
%%====================================================================

%% @private
generate_is_enabled(#{preset := PresetMap}) when map_size(PresetMap) > 0 ->
    Clauses = maps:fold(fun(Trsn, PresetList, Acc) ->
        %% Match Mode has at least one token in each preset place
        MatchParts = [<<(atom_to_source(P))/binary, " := [_ | _]">> || P <- PresetList],
        MatchStr = iolist_to_binary(lists:join(<<",\n           ">>, MatchParts)),
        [<< "\n        {", (atom_to_source(Trsn))/binary, ", #{", MatchStr/binary, "}, _UsrInfo} ->\n            true" >> | Acc]
    end, [], PresetMap),
    JoinedClauses = iolist_to_binary(lists:join(<<";">>, Clauses ++ [<<"\n        {_Transition, _Mode, _UsrInfo} ->\n            false">>])),
    iolist_to_binary([
        <<"%% @doc Checks if a transition is enabled in the given mode.\n"
           "%% @private\n"
           "-spec is_enabled(atom(), #{atom() => [term()]}, term()) -> boolean().\n"
           "is_enabled(Transition, Mode, UsrInfo) ->\n"
           "    case {Transition, Mode, UsrInfo} of">>,
        JoinedClauses,
        <<"\n    end.\n">>
    ]);
generate_is_enabled(#{input_condition := InputCond, tasks := Tasks}) ->
    InputPlace = sanitize_atom_name(InputCond),
    Clauses = [ generate_is_enabled_clause(Task, InputPlace) || Task <- Tasks ] ++
              [ <<"\n        {_Transition, _Mode, _UsrInfo} ->\n            false">> ],
    JoinedClauses = iolist_to_binary(lists:join(<<";">>, Clauses)),
    iolist_to_binary([
        <<"%% @doc Checks if a transition is enabled in the given mode.\n"
           "%% @private\n"
           "-spec is_enabled(atom(), #{atom() => [term()]}, term()) -> boolean().\n"
           "is_enabled(Transition, Mode, UsrInfo) ->\n"
           "    case {Transition, Mode, UsrInfo} of">>,
        JoinedClauses,
        <<"\n    end.\n">>
    ]).

%% @private
generate_is_enabled_clause(TaskId, InputPlace) ->
    Transition = transition_atom(TaskId, <<"t_">>),
    TaskPlace = sanitize_atom_name(TaskId),

    %%
    %% The is_enabled clause checks for:
    %% 1. At least one token in the input place (workflow start)
    %% 2. At least one token in the task place (task activation)
    %%
    Clause = <<
        "\n        {", (atom_to_source(Transition))/binary,
        ", #{", (atom_to_source(InputPlace))/binary, " := [InputToken | _],\n"
        "           ", (atom_to_source(TaskPlace))/binary, " := [TaskToken | _]}, _UsrInfo} ->\n"
        "            InputToken =/= undefined andalso TaskToken =/= undefined"
    >>,
    Clause.

%%====================================================================
%% Internal Functions - Code Generation - fire/3
%%====================================================================

%% @private
generate_fire(#{preset := PresetMap, postset := PostsetMap}) when map_size(PresetMap) > 0, map_size(PostsetMap) > 0 ->
    Clauses = maps:fold(fun(Trsn, PresetList, Acc) ->
        PostsetList = maps:get(Trsn, PostsetMap, []),
        [generate_fire_expanded_clause(Trsn, PresetList, PostsetList) | Acc]
    end, [], PresetMap),
    JoinedClauses = iolist_to_binary(lists:join(<<";">>, Clauses ++ [<<"\n        {_Transition, _Mode, _UsrInfo} ->\n            abort">>])),
    iolist_to_binary([
        <<"%% @doc Fires a transition, consuming and producing tokens.\n"
           "%% @private\n"
           "-spec fire(atom(), #{atom() => [term()]}, term()) ->\n"
           "    {produce, #{atom() => [term()]}} | abort.\n"
           "fire(Transition, Mode, UsrInfo) ->\n"
           "    case {Transition, Mode, UsrInfo} of">>,
        JoinedClauses,
        <<"\n    end.\n">>
    ]);
generate_fire(#{input_condition := InputCond,
                output_condition := OutputCond,
                tasks := Tasks,
                split_types := SplitTypes}) ->
    InputPlace = sanitize_atom_name(InputCond),
    OutputPlace = sanitize_atom_name(OutputCond),

    Clauses = [ generate_fire_clause(Task, InputPlace, OutputPlace, SplitTypes) || Task <- Tasks ] ++
              [ <<"\n        {_Transition, _Mode, _UsrInfo} ->\n            abort">> ],

    JoinedClauses = iolist_to_binary(lists:join(<<";">>, Clauses)),
    iolist_to_binary([
        <<"%% @doc Fires a transition, consuming and producing tokens.\n"
           "%% @private\n"
           "-spec fire(atom(), #{atom() => [term()]}, term()) ->\n"
           "    {produce, #{atom() => [term()]}} | abort.\n"
           "fire(Transition, Mode, UsrInfo) ->\n"
           "    case {Transition, Mode, UsrInfo} of">>,
        JoinedClauses,
        <<"\n    end.\n">>
    ]).

%% @private
-spec generate_fire_expanded_clause(atom(), [place()], [place()]) -> binary().
generate_fire_expanded_clause(Trsn, PresetList, PostsetList) ->
    %% Match Mode: each preset place has at least one token
    MatchParts = [<<(atom_to_source(P))/binary, " := [_]">> || P <- PresetList],
    MatchStr = iolist_to_binary(lists:join(<<",\n           ">>, MatchParts)),
    %% Produce map: each postset place gets token (t_split->token, t_finish*/t_complete*->done, t_merge->merged)
    Token = produce_token_for_transition(Trsn),
    ProduceParts = [<<(atom_to_source(P))/binary, " => [", (atom_to_source(Token))/binary, "]">> || P <- PostsetList],
    ProduceStr = iolist_to_binary(lists:join(<<",\n           ">>, ProduceParts)),
    <<"\n        {", (atom_to_source(Trsn))/binary, ", #{", MatchStr/binary, "}, _UsrInfo} ->\n"
      "        {produce, #{\n           ", ProduceStr/binary, "\n        }}">>.

%% @private Token produced by transition (thread_split/thread_merge pattern)
produce_token_for_transition(t_split) -> token;
produce_token_for_transition(t_finish) -> done;
produce_token_for_transition(t_finish1) -> done;
produce_token_for_transition(t_finish2) -> done;
produce_token_for_transition(t_finish3) -> done;
produce_token_for_transition(t_finish4) -> done;
produce_token_for_transition(t_complete1) -> done;
produce_token_for_transition(t_complete2) -> done;
produce_token_for_transition(t_complete3) -> done;
produce_token_for_transition(t_complete4) -> done;
produce_token_for_transition(t_merge) -> merged;
produce_token_for_transition(_) -> token.

%% @private
-spec generate_fire_clause(task_id(), place(), place(), #{task_id() => atom()}) -> binary().
generate_fire_clause(TaskId, InputPlace, OutputPlace, SplitTypes) ->
    Transition = transition_atom(TaskId, <<"t_">>),
    TaskPlace = sanitize_atom_name(TaskId),

    _SplitType = maps:get(TaskId, SplitTypes, ?XOR),

    %%
    %% The fire clause:
    %% 1. Consumes the input token
    %% 2. Consumes the task token
    %% 3. Produces a completion token to the output place
    %% 4. For AND splits, produces tokens to all outgoing flows
    %% 5. For XOR splits, produces token to one outgoing flow
    %% 6. For OR splits, produces tokens to selected flows
    %%

    BaseClause = <<
        "\n        {", (atom_to_source(Transition))/binary,
        ", #{", (atom_to_source(InputPlace))/binary, " := [InputToken],\n"
        "           ", (atom_to_source(TaskPlace))/binary, " := [TaskToken]}, _UsrInfo} ->\n"
        "        %% Fire task '", (escape_binary(atom_to_binary(TaskId, utf8)))/binary, "'\n"
        "        {produce, #{\n"
        "           ", (atom_to_source(OutputPlace))/binary, " => [done]\n"
        "        }}"
    >>,

    BaseClause.

%%====================================================================
%% Internal Functions - File Writing
%%====================================================================

%% @private
write_modules_to_files(Modules, OutputDir, _SpecId) ->
    %% Ensure directory exists
    case filelib:is_dir(OutputDir) of
        true ->
            ok;
        false ->
            case file:make_dir(OutputDir) of
                ok -> ok;
                {error, eexist} -> ok; %% Already created by another process
                {error, _DirReason} ->
                    ok
            end
    end,
    Results = maps:fold(fun(NetId, ModuleCode, Acc) ->
        FileName = module_filename(NetId, OutputDir),
        FilePath = filename:join(OutputDir, FileName),
        case file:write_file(FilePath, ModuleCode) of
            ok ->
                [FilePath | Acc];
            {error, WriteReason} ->
                throw({file_write_error, FilePath, WriteReason})
        end
    end, [], Modules),
    {ok, lists:reverse(Results)}.

%% @private
module_filename(NetId, _OutputDir) ->
    SafeName = sanitize_filename(NetId),
    <<SafeName/binary, ".erl">>.

%% @private
sanitize_filename(NetId) ->
    lists:foldl(fun(C, Acc) ->
        case C of
            $/ -> <<Acc/binary, $_>>;
            $\\ -> <<Acc/binary, $_>>;
            $: -> <<Acc/binary, $_>>;
            $* -> <<Acc/binary, $_>>;
            $? -> <<Acc/binary, $_>>;
            $< -> <<Acc/binary, $_>>;
            $> -> <<Acc/binary, $_>>;
            $| -> <<Acc/binary, $_>>;
            $" -> <<Acc/binary, $_>>;
            C when C >= 0, C =< 255 -> <<Acc/binary, C>>;
            _ -> <<Acc/binary, $_>>
        end
    end, <<>>, binary_to_list(NetId)).

%%====================================================================
%% Internal Functions - Place/Transition Atom Generation
%%====================================================================

%% @private
-spec sanitize_atom_name(binary() | atom() | list()) -> atom().

sanitize_atom_name(Name) when is_atom(Name) ->
    sanitize_atom_name(atom_to_binary(Name, utf8));
sanitize_atom_name(Name) when is_list(Name) ->
    sanitize_atom_name(list_to_binary(Name));
sanitize_atom_name(Name) when is_binary(Name) ->
    %% Convert binary to safe Erlang atom
    %% Replace invalid characters with underscore
    SafeName = sanitize_atom_chars(Name),
    %% Ensure it starts with a letter
    FinalName = case SafeName of
        <<First, _/binary>> when First >= $a, First =< $z -> SafeName;
        <<First, _/binary>> when First >= $A, First =< $Z -> SafeName;
        <<First, _/binary>> when First >= $_, First =< $9 ->
            <<$_, SafeName/binary>>;
        _ ->
            <<"net_", SafeName/binary>>
    end,
    binary_to_atom(FinalName, utf8).

%% @private
sanitize_atom_chars(<<>>) ->
    <<>>;
sanitize_atom_chars(<<C, Rest/binary>>) ->
    RestSafe = sanitize_atom_chars(Rest),
    case valid_atom_char(C) of
        true -> <<C, RestSafe/binary>>;
        false -> <<$_, RestSafe/binary>>
    end.

%% @private
valid_atom_char(C) when C >= $a, C =< $z -> true;
valid_atom_char(C) when C >= $A, C =< $Z -> true;
valid_atom_char(C) when C >= $0, C =< $9 -> true;
valid_atom_char($_) -> true;
valid_atom_char($@) -> true;
valid_atom_char(_) -> false.

%% @private
-spec place_atom(binary() | atom(), binary()) -> atom().

place_atom(Base, Suffix) when is_atom(Base) ->
    place_atom(atom_to_binary(Base, utf8), Suffix);
place_atom(Base, Suffix) when is_binary(Base) ->
    Name = <<Base/binary, Suffix/binary>>,
    sanitize_atom_name(Name).

%% @private
-spec transition_atom(binary() | atom(), binary()) -> atom().

transition_atom(Base, Prefix) when is_atom(Base) ->
    transition_atom(atom_to_binary(Base, utf8), Prefix);
transition_atom(Base, Prefix) when is_binary(Base) ->
    Name = <<Prefix/binary, Base/binary>>,
    sanitize_atom_name(Name).

%%====================================================================
%% Internal Functions - Flow Map Building
%%====================================================================

%% @private
-spec build_flow_map(spec()) -> #{binary() => [{task_id(), task_id()}]}.

build_flow_map(Spec) ->
    Flows = wf_spec:flows(Spec),
    lists:foldl(fun({From, To, _Pred}, Acc) ->
        FromBin = atom_to_binary(From, utf8),
        Existing = maps:get(FromBin, Acc, []),
        Acc#{FromBin => [{From, To} | Existing]}
    end, #{}, Flows).

%% @private
-spec extract_net_tasks(spec(), binary()) -> [task_id()].

extract_net_tasks(Spec, NetId) ->
    case wf_spec:decomposition_tasks(Spec, NetId) of
        {ok, Tasks} -> Tasks;
        {error, _} -> []
    end.

%% @private
-spec generate_preset_clauses(net_info(), [place()]) -> binary().

generate_preset_clauses(#{tasks := Tasks}, _AllPlaces) ->
    lists:map(fun(TaskId) ->
        Transition = transition_atom(TaskId, <<"t_">>),
        TaskPlace = sanitize_atom_name(TaskId),
        <<
            "preset(", (atom_to_binary(Transition, utf8))/binary,
            ") ->\n    [", (atom_to_binary(TaskPlace, utf8))/binary, "];\n"
        >>
    end, Tasks).

%% @private
-spec generate_fire_clauses(net_info(), place(), binary()) -> binary().

generate_fire_clauses(#{tasks := Tasks}, OutputPlace, _NetId) ->
    lists:map(fun(TaskId) ->
        Transition = transition_atom(TaskId, <<"t_">>),
        TaskPlace = sanitize_atom_name(TaskId),
        <<
            "fire(", (atom_to_binary(Transition, utf8))/binary,
            ", #{", (atom_to_binary(TaskPlace, utf8))/binary, " := [_]}, _) ->\n"
            "    {produce, #{", (atom_to_binary(OutputPlace, utf8))/binary,
            " => [done]}};\n"
        >>
    end, Tasks).

%% @private
extract_places_and_transitions(Modules, NetInfos) ->
    maps:fold(fun(NetId, _ModuleCode, {PlacesAcc, TransAcc}) ->
        NetInfo = maps:get(NetId, NetInfos),
        Places = generate_places(NetInfo),
        Transitions = generate_transitions(NetInfo),
        {PlacesAcc#{NetId => Places}, TransAcc#{NetId => Transitions}}
    end, {#{}, #{}}, Modules).

%%====================================================================
%% Doctests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% @doc Runs all doctests for the module.
%% @private
doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

%% Test sanitize_atom_name with various inputs
sanitize_atom_name_binary_test() ->
    ?assertEqual('test_name', sanitize_atom_name(<<"test_name">>)),
    ?assertEqual('test_name', sanitize_atom_name(<<"test-name">>)),
    ?assertEqual('test_name', sanitize_atom_name(<<"test name">>)),
    ?assertEqual('test123', sanitize_atom_name(<<"test123">>)),
    ?assertEqual('net_123', sanitize_atom_name(<<"123">>)).

sanitize_atom_name_atom_test() ->
    ?assertEqual('test_name', sanitize_atom_name(test_name)),
    ?assertEqual('test_name', sanitize_atom_name('test-name')).

sanitize_atom_name_list_test() ->
    ?assertEqual('test_name', sanitize_atom_name("test_name")),
    ?assertEqual('test_name', sanitize_atom_name("test-name")).

%% Test place_atom generation
place_atom_test() ->
    ?assertEqual('test_input', place_atom(<<"test">>, <<"_input">>)),
    ?assertEqual('test_output', place_atom(<<"test">>, <<"_output">>)),
    ?assertEqual('my_net_input', place_atom(<<"my_net">>, <<"_input">>)).

%% Test transition_atom generation
transition_atom_test() ->
    ?assertEqual('t_task1', transition_atom(<<"task1">>, <<"t_">>)),
    ?assertEqual('tr_task1', transition_atom(<<"task1">>, <<"tr_">>)),
    ?assertEqual('t_my_task', transition_atom(<<"my_task">>, <<"t_">>)).

%% Test generate_places
generate_places_test() ->
    NetInfo = #{
        input_condition => <<"net_input">>,
        output_condition => <<"net_output">>,
        tasks => [task1, task2]
    },
    Places = generate_places(NetInfo),
    ?assertEqual(true, is_list(Places)),
    ?assertEqual(4, length(Places)),
    ?assertEqual(true, lists:member('net_input', Places)),
    ?assertEqual(true, lists:member('net_output', Places)).

%% Test generate_transitions
generate_transitions_test() ->
    NetInfo = #{
        tasks => [task1, task2, task3]
    },
    Transitions = generate_transitions(NetInfo),
    ?assertEqual(true, is_list(Transitions)),
    ?assertEqual(3, length(Transitions)),
    ?assertEqual(true, lists:member('t_task1', Transitions)),
    ?assertEqual(true, lists:member('t_task2', Transitions)).

%% Test module_name generation
module_name_test() ->
    Options = #{module_prefix => <<"yawl_">>},
    ?assertEqual('yawl_test', module_name(<<"test">>, Options)),
    ?assertEqual('yawl_my_net', module_name(<<"my_net">>, Options)),

    Options2 = #{module_prefix => <<"workflow_">>},
    ?assertEqual('workflow_main', module_name(<<"main">>, Options2)).

%% Test escape_binary
escape_binary_test() ->
    ?assertEqual(<<"hello">>, escape_binary(<<"hello">>)),
    ?assertEqual(<<"hello\\nworld">>, escape_binary(<<"hello\nworld">>)),
    ?assertEqual(<<"quote\\\"test">>, escape_binary(<<"quote\"test">>)).

%% Test sanitize_filename - just sanitizes, doesn't add extension
sanitize_filename_test() ->
    ?assertEqual(<<"test_file">>, sanitize_filename(<<"test/file">>)),
    ?assertEqual(<<"test_file">>, sanitize_filename(<<"test\\file">>)),
    ?assertEqual(<<"test_file">>, sanitize_filename(<<"test*file">>)).

%% Test module_filename - adds .erl extension
module_filename_test() ->
    ?assertEqual(<<"test_file.erl">>, module_filename(<<"test/file">>, <<"outdir">>)),
    ?assertEqual(<<"test_file.erl">>, module_filename(<<"test\\file">>, <<"outdir">>)),
    ?assertEqual(<<"test_file.erl">>, module_filename(<<"test*file">>, <<"outdir">>)).

%% Test normalize_options
normalize_options_test() ->
    Empty = normalize_options(#{}),
    ?assertEqual(0, maps:get(seed, Empty)),
    ?assertEqual(<<"yawl_">>, maps:get(module_prefix, Empty)),

    WithOpts = normalize_options(#{seed => 42, module_prefix => <<"test_">>}),
    ?assertEqual(42, maps:get(seed, WithOpts)),
    ?assertEqual(<<"test_">>, maps:get(module_prefix, WithOpts)).

-endif.
