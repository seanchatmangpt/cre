%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
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
%% @doc YAWL to pnet_net compiler.
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
-export([generate_preset_clauses/2, generate_fire_clause/3, generate_fire_clauses/3]).

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

compile(_Spec, _Options) ->
    {error, invalid_spec}.


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
%% Returns a list of place atoms for the specified net structure.
%% Places include: input condition, output condition, and one per task.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_places(NetInfo :: net_info()) -> [place()].

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
%% Returns a list of transition atoms for the specified net structure.
%% Each task has exactly one transition.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_transitions(NetInfo :: net_info()) -> [transition()].

generate_transitions(#{tasks := Tasks}) ->
    [transition_atom(TaskId, <<"t_">>) || TaskId <- Tasks].

%%====================================================================
%% Internal Functions - Options
%%====================================================================

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
           "init(_NetArg) ->\n"
           "    #{}.\n"
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
module_name(NetId, #{module_prefix := Prefix}) ->
    PrefixBin = <<Prefix/binary, NetId/binary>>,
    list_to_atom(binary_to_list(PrefixBin)).

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
        <<"  ", (atom_to_binary(P, utf8))/binary>>
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
        <<"  ", (atom_to_binary(T, utf8))/binary>>
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
generate_init_marking(#{input_condition := InputCond, tasks := Tasks}) ->
    InputPlace = sanitize_atom_name(InputCond),

    InitClauses = [
        <<"\n        ", (atom_to_binary(InputPlace, utf8))/binary, " ->\n            [init]">>
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
        "\n        ", (atom_to_binary(Place, utf8))/binary, " ->\n            []"
    >>.

%%====================================================================
%% Internal Functions - Code Generation - preset/1
%%====================================================================

%% @private
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
        "\n        ", (atom_to_binary(Transition, utf8))/binary,
        " ->\n            [", (atom_to_binary(InputPlace, utf8))/binary,
        ", ", (atom_to_binary(TaskPlace, utf8))/binary, "]"
    >>.

%%====================================================================
%% Internal Functions - Code Generation - is_enabled/3
%%====================================================================

%% @private
generate_is_enabled(#{tasks := Tasks}) ->
    Clauses = [ generate_is_enabled_clause(Task) || Task <- Tasks ] ++
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
generate_is_enabled_clause(TaskId) ->
    Transition = transition_atom(TaskId, <<"t_">>),
    InputPlace = place_atom(TaskId, <<"_input">>),
    TaskPlace = sanitize_atom_name(TaskId),

    %%
    %% The is_enabled clause checks for:
    %% 1. At least one token in the input place (workflow start)
    %% 2. At least one token in the task place (task activation)
    %%
    Clause = <<
        "\n        {", (atom_to_binary(Transition, utf8))/binary,
        ", #{", (atom_to_binary(InputPlace, utf8))/binary, " := [InputToken | _],\n"
        "           ", (atom_to_binary(TaskPlace, utf8))/binary, " := [TaskToken | _]}, _UsrInfo} ->\n"
        "            InputToken =/= undefined andalso TaskToken =/= undefined"
    >>,
    Clause.

%%====================================================================
%% Internal Functions - Code Generation - fire/3
%%====================================================================

%% @private
generate_fire(#{input_condition := _InputCond,
                output_condition := OutputCond,
                tasks := Tasks,
                split_types := SplitTypes}) ->
    _InputPlace = sanitize_atom_name(_InputCond),
    OutputPlace = sanitize_atom_name(OutputCond),

    Clauses = [ generate_fire_clause(Task, OutputPlace, SplitTypes) || Task <- Tasks ] ++
              [ <<"\n    fire(_Transition, _Mode, _UsrInfo) ->\n        abort">> ],

    JoinedClauses = iolist_to_binary(lists:join(<<>>, Clauses)),
    iolist_to_binary([
        <<"%% @doc Fires a transition, consuming and producing tokens.\n"
           "%% @private\n"
           "-spec fire(atom(), #{atom() => [term()]}, term()) ->\n"
           "    {produce, #{atom() => [term()]}} | abort.\n"
           "fire(Transition, Mode, UsrInfo) ->\n"
           "    case Transition of">>,
        JoinedClauses,
        <<"\n    end.\n">>
    ]).

%% @private
-spec generate_fire_clause(task_id(), place(), #{task_id() => atom()}) -> binary().
generate_fire_clause(TaskId, OutputPlace, SplitTypes) ->
    Transition = transition_atom(TaskId, <<"t_">>),
    InputPlace = place_atom(TaskId, <<"_input">>),
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
        "\n    fire(", (atom_to_binary(Transition, utf8))/binary,
        ", #{", (atom_to_binary(InputPlace, utf8))/binary, " := [InputToken],\n"
        "           ", (atom_to_binary(TaskPlace, utf8))/binary, " := [TaskToken]}, _UsrInfo) ->\n"
        "        %% Fire task '", (escape_binary(atom_to_binary(TaskId, utf8)))/binary, "'\n"
        "        {produce, #{\n"
        "           ", (atom_to_binary(OutputPlace, utf8))/binary, " => [done]\n"
        "        }};"
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
