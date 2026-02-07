%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @doc YAWL Worklet Service for Dynamic Workflow Adaptation.
%%
%% This module implements the YAWL worklet pattern for dynamic workflow
%% adaptation based on exceptions, data conditions, and time triggers.
%%
%% <h3>Worklet Features</h3>
%% <ul>
%%   <li><b>Exception Handling</b> - Dynamic response to workflow exceptions</li>
%%   <li><b>Data-Driven Selection</b> - Worklet selection based on data values</li>
%%   <li><b>Time Triggers</b> - Scheduled worklet activation</li>
%%   <li><b>Pre/Post Conditions</b> - Conditional worklet execution</li>
%% </ul>
%%
%% <h3>Selection Policies</h3>
%% <ul>
%%   <li><b>auto_offer</b> - Suggest worklet to user for approval</li>
%%   <li><b>auto_start</b> - Automatically execute the worklet</li>
%%   <li><b>manual</b> - Require manual intervention</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_worklet).
-behavior(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([start_link/0, start_link/1]).
-export([register_worklet/2, unregister_worklet/1]).
-export([add_worklet_mapping/3, remove_worklet_mapping/1]).
-export([check_preconditions/2, check_postconditions/2]).
-export([select_worklet/2, launch_worklet/3]).
-export([get_active_worklets/0, get_completed_worklets/0]).
-export([trigger_worklet/2, abort_worklet/1]).
-export([get_worklet_spec/1, list_worklet_specs/0]).
-export([set_worklet_parameter/3, get_worklet_parameters/1]).
-export([complete_worklet_instance/2]).

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
         handle_info/2, init/1, terminate/2]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type selection_policy() :: auto_offer | auto_start | manual.
-type trigger_type() :: exception | data | time.
-type worklet_status() :: idle | running | completed | aborted | failed.

%%====================================================================
%% Record Definitions
%%====================================================================

-record(worklet_spec, {
    id :: binary(),
    name :: binary(),
    parameters :: [binary()],
    pre_condition :: function() | undefined,
    post_condition :: function() | undefined,
    constraints :: #{atom() => term()},
    workflow_def :: term() | undefined,
    description :: binary() | undefined
}).

-record(worklet_mapping, {
    id :: binary(),
    task_id :: binary(),
    worklet_spec_id :: binary(),
    selection_policy :: selection_policy(),
    triggered_by :: trigger_type(),
    trigger_condition :: term() | undefined,
    priority :: non_neg_integer()
}).

-record(worklet_instance, {
    instance_id :: binary(),
    worklet_spec_id :: binary(),
    task_id :: binary(),
    status :: worklet_status(),
    started_at :: erlang:timestamp() | undefined,
    completed_at :: erlang:timestamp() | undefined,
    parameters :: #{binary() => term()},
    result :: term() | undefined
}).

-record(worklet_state, {
    specs = #{} :: #{binary() => #worklet_spec{}},
    mappings = [] :: [#worklet_mapping{}],
    instances = [] :: [#worklet_instance{}],
    spec_counter = 0 :: non_neg_integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Starts an anonymous worklet service.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @doc Starts a named worklet service.
start_link(Name) ->
    gen_server:start_link(Name, ?MODULE, [], []).

%% @doc Registers a new worklet specification.
-spec register_worklet(binary(), [{atom(), term()}]) ->
    {ok, binary()} | {error, term()}.
register_worklet(Name, Props) ->
    gen_server:call(?MODULE, {register_worklet, Name, Props}).

%% @doc Unregisters a worklet specification.
-spec unregister_worklet(binary()) -> ok | {error, term()}.
unregister_worklet(SpecId) ->
    gen_server:call(?MODULE, {unregister_worklet, SpecId}).

%% @doc Adds a worklet mapping linking a task to a worklet.
-spec add_worklet_mapping(binary(), binary(), selection_policy()) ->
    {ok, binary()} | {error, term()}.
add_worklet_mapping(TaskId, WorkletSpecId, Policy) ->
    gen_server:call(?MODULE, {add_worklet_mapping, TaskId, WorkletSpecId, Policy}).

%% @doc Removes a worklet mapping.
-spec remove_worklet_mapping(binary()) -> ok | {error, term()}.
remove_worklet_mapping(MappingId) ->
    gen_server:call(?MODULE, {remove_worklet_mapping, MappingId}).

%% @doc Checks if preconditions are met for a worklet.
-spec check_preconditions(binary(), map()) -> {ok, boolean()} | {error, term()}.
check_preconditions(SpecId, Context) ->
    gen_server:call(?MODULE, {check_preconditions, SpecId, Context}).

%% @doc Checks if postconditions are met for a worklet.
-spec check_postconditions(binary(), map()) -> {ok, boolean()} | {error, term()}.
check_postconditions(SpecId, Context) ->
    gen_server:call(?MODULE, {check_postconditions, SpecId, Context}).

%% @doc Selects an appropriate worklet for a task.
-spec select_worklet(binary(), trigger_type()) ->
    {ok, binary()} | {error, term()}.
select_worklet(TaskId, Trigger) ->
    gen_server:call(?MODULE, {select_worklet, TaskId, Trigger}).

%% @doc Launches a worklet instance for a task.
-spec launch_worklet(binary(), binary(), map()) ->
    {ok, binary()} | {error, term()}.
launch_worklet(TaskId, SpecId, Parameters) ->
    gen_server:call(?MODULE, {launch_worklet, TaskId, SpecId, Parameters}).

%% @doc Gets all active worklet instances.
-spec get_active_worklets() -> [#worklet_instance{}].
get_active_worklets() ->
    gen_server:call(?MODULE, get_active_worklets).

%% @doc Gets all completed worklet instances.
-spec get_completed_worklets() -> [#worklet_instance{}].
get_completed_worklets() ->
    gen_server:call(?MODULE, get_completed_worklets).

%% @doc Triggers a worklet based on an event.
-spec trigger_worklet(binary(), trigger_type()) ->
    {ok, binary()} | {error, term()}.
trigger_worklet(TaskId, Trigger) ->
    gen_server:call(?MODULE, {trigger_worklet, TaskId, Trigger}).

%% @doc Aborts a running worklet instance.
-spec abort_worklet(binary()) -> ok | {error, term()}.
abort_worklet(InstanceId) ->
    gen_server:call(?MODULE, {abort_worklet, InstanceId}).

%% @doc Completes a running worklet instance with a result.
-spec complete_worklet_instance(binary(), term()) -> ok | {error, term()}.
complete_worklet_instance(InstanceId, Result) ->
    gen_server:call(?MODULE, {complete_worklet_instance, InstanceId, Result}).

%% @doc Gets a worklet specification by ID.
-spec get_worklet_spec(binary()) -> {ok, #worklet_spec{}} | {error, not_found}.
get_worklet_spec(SpecId) ->
    gen_server:call(?MODULE, {get_worklet_spec, SpecId}).

%% @doc Lists all registered worklet specifications.
-spec list_worklet_specs() -> [#worklet_spec{}].
list_worklet_specs() ->
    gen_server:call(?MODULE, list_worklet_specs).

%% @doc Sets a parameter value for a worklet spec.
-spec set_worklet_parameter(binary(), binary(), term()) -> ok | {error, term()}.
set_worklet_parameter(SpecId, ParamName, Value) ->
    gen_server:call(?MODULE, {set_worklet_parameter, SpecId, ParamName, Value}).

%% @doc Gets all parameters for a worklet spec.
-spec get_worklet_parameters(binary()) -> {ok, [binary()]} | {error, term()}.
get_worklet_parameters(SpecId) ->
    gen_server:call(?MODULE, {get_worklet_parameters, SpecId}).

%%====================================================================
%% gen_server Callback Functions
%%====================================================================

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({register_worklet, Name, Props}, _From, State) ->
    #worklet_state{specs = Specs} = State,

    SpecId = generate_id(<<"worklet_spec">>),
    Parameters = proplists:get_value(parameters, Props, []),
    PreCondition = proplists:get_value(pre_condition, Props, undefined),
    PostCondition = proplists:get_value(post_condition, Props, undefined),
    Constraints = proplists:get_value(constraints, Props, #{}),
    WorkflowDef = proplists:get_value(workflow_def, Props, undefined),
    Description = proplists:get_value(description, Props, undefined),

    Spec = #worklet_spec{
        id = SpecId,
        name = Name,
        parameters = ensure_binary_list(Parameters),
        pre_condition = PreCondition,
        post_condition = PostCondition,
        constraints = Constraints,
        workflow_def = WorkflowDef,
        description = Description
    },

    {reply, {ok, SpecId}, State#worklet_state{
        specs = Specs#{SpecId => Spec}
    }};

handle_call({unregister_worklet, SpecId}, _From, State) ->
    #worklet_state{specs = Specs} = State,

    case maps:is_key(SpecId, Specs) of
        true ->
            {reply, ok, State#worklet_state{
                specs = maps:remove(SpecId, Specs)
            }};
        false ->
            {reply, {error, not_found}, State}
    end;

handle_call({add_worklet_mapping, TaskId, WorkletSpecId, Policy}, _From, State) ->
    #worklet_state{specs = Specs, mappings = Mappings} = State,

    case maps:is_key(WorkletSpecId, Specs) of
        false ->
            {reply, {error, worklet_spec_not_found}, State};
        true ->
            MappingId = generate_id(<<"mapping">>),
            Mapping = #worklet_mapping{
                id = MappingId,
                task_id = TaskId,
                worklet_spec_id = WorkletSpecId,
                selection_policy = Policy,
                triggered_by = exception,
                trigger_condition = undefined,
                priority = 0
            },
            {reply, {ok, MappingId}, State#worklet_state{
                mappings = [Mapping | Mappings]
            }}
    end;

handle_call({remove_worklet_mapping, MappingId}, _From, State) ->
    #worklet_state{mappings = Mappings} = State,

    UpdatedMappings = lists:filter(
        fun(M) -> M#worklet_mapping.id =/= MappingId end,
        Mappings
    ),

    {reply, ok, State#worklet_state{mappings = UpdatedMappings}};

handle_call({check_preconditions, SpecId, Context}, _From, State) ->
    #worklet_state{specs = Specs} = State,

    case maps:get(SpecId, Specs, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #worklet_spec{pre_condition = undefined} ->
            {reply, {ok, true}, State};
        #worklet_spec{pre_condition = PreCondFun} when is_function(PreCondFun, 0) ->
            try
                Result = PreCondFun(),
                {reply, {ok, Result}, State}
            catch
                _:_ ->
                    {reply, {error, precondition_failed}, State}
            end;
        #worklet_spec{pre_condition = PreCondFun} when is_function(PreCondFun, 1) ->
            try
                Result = PreCondFun(Context),
                {reply, {ok, Result}, State}
            catch
                _:_ ->
                    {reply, {error, precondition_failed}, State}
            end;
        _ ->
            {reply, {ok, true}, State}
    end;

handle_call({check_postconditions, SpecId, Context}, _From, State) ->
    #worklet_state{specs = Specs} = State,

    case maps:get(SpecId, Specs, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #worklet_spec{post_condition = undefined} ->
            {reply, {ok, true}, State};
        #worklet_spec{post_condition = PostCondFun} when is_function(PostCondFun, 0) ->
            try
                Result = PostCondFun(),
                {reply, {ok, Result}, State}
            catch
                _:_ ->
                    {reply, {error, postcondition_failed}, State}
            end;
        #worklet_spec{post_condition = PostCondFun} when is_function(PostCondFun, 1) ->
            try
                Result = PostCondFun(Context),
                {reply, {ok, Result}, State}
            catch
                _:_ ->
                    {reply, {error, postcondition_failed}, State}
            end;
        _ ->
            {reply, {ok, true}, State}
    end;

handle_call({select_worklet, TaskId, Trigger}, _From, State) ->
    #worklet_state{mappings = Mappings} = State,

    %% Find mappings for the task and trigger type
    CandidateMappings = lists:filter(
        fun(M) ->
            M#worklet_mapping.task_id =:= TaskId andalso
            M#worklet_mapping.triggered_by =:= Trigger
        end,
        Mappings
    ),

    case CandidateMappings of
        [] ->
            {reply, {error, no_worklet_found}, State};
        _ ->
            %% Select by priority (highest first)
            Sorted = lists:sort(
                fun(A, B) ->
                    A#worklet_mapping.priority >= B#worklet_mapping.priority
                end,
                CandidateMappings
            ),
            #worklet_mapping{worklet_spec_id = SpecId} = hd(Sorted),
            {reply, {ok, SpecId}, State}
    end;

handle_call({launch_worklet, TaskId, SpecId, Parameters}, _From, State) ->
    #worklet_state{specs = Specs, instances = Instances} = State,

    case maps:get(SpecId, Specs, undefined) of
        undefined ->
            {reply, {error, worklet_spec_not_found}, State};
        Spec ->
            InstanceId = generate_id(<<"worklet_instance">>),
            Instance = #worklet_instance{
                instance_id = InstanceId,
                worklet_spec_id = SpecId,
                task_id = TaskId,
                status = running,
                started_at = erlang:timestamp(),
                completed_at = undefined,
                parameters = Parameters,
                result = undefined
            },

            %% Check preconditions before launching
            case catch check_precondition_safe(Spec, Parameters) of
                true ->
                    {reply, {ok, InstanceId}, State#worklet_state{
                        instances = [Instance | Instances]
                    }};
                false ->
                    {reply, {error, precondition_not_met}, State};
                _ ->
                    {reply, {error, precondition_error}, State}
            end
    end;

handle_call(get_active_worklets, _From, State) ->
    #worklet_state{instances = Instances} = State,

    Active = lists:filter(
        fun(I) -> I#worklet_instance.status =:= running end,
        Instances
    ),

    {reply, Active, State};

handle_call(get_completed_worklets, _From, State) ->
    #worklet_state{instances = Instances} = State,

    Completed = lists:filter(
        fun(I) -> I#worklet_instance.status =:= completed end,
        Instances
    ),

    {reply, Completed, State};

handle_call({trigger_worklet, TaskId, Trigger}, _From, State) ->
    #worklet_state{mappings = Mappings} = State,

    %% Find applicable mappings
    CandidateMappings = lists:filter(
        fun(M) ->
            M#worklet_mapping.task_id =:= TaskId andalso
            M#worklet_mapping.triggered_by =:= Trigger
        end,
        Mappings
    ),

    case CandidateMappings of
        [] ->
            {reply, {error, no_worklet_found}, State};
        [Mapping | _] ->
            case Mapping#worklet_mapping.selection_policy of
                auto_start ->
                    SpecId = Mapping#worklet_mapping.worklet_spec_id,
                    #worklet_state{specs = Specs, instances = Instances} = State,
                    case maps:get(SpecId, Specs, undefined) of
                        undefined ->
                            {reply, {error, worklet_spec_not_found}, State};
                        _Spec ->
                            InstanceId = generate_id(<<"worklet_instance">>),
                            Instance = #worklet_instance{
                                instance_id = InstanceId,
                                worklet_spec_id = SpecId,
                                task_id = TaskId,
                                status = running,
                                started_at = erlang:timestamp(),
                                completed_at = undefined,
                                parameters = #{},
                                result = undefined
                            },
                            {reply, {ok, InstanceId}, State#worklet_state{
                                instances = [Instance | Instances]
                            }}
                    end;
                _ ->
                    {reply, {ok, Mapping#worklet_mapping.id}, State}
            end
    end;

handle_call({abort_worklet, InstanceId}, _From, State) ->
    #worklet_state{instances = Instances} = State,

    UpdatedInstances = lists:map(
        fun(I) when I#worklet_instance.instance_id =:= InstanceId ->
                I#worklet_instance{status = aborted,
                                   completed_at = erlang:timestamp()};
           (I) ->
                I
        end,
        Instances
    ),

    {reply, ok, State#worklet_state{instances = UpdatedInstances}};

handle_call({complete_worklet_instance, InstanceId, Result}, _From, State) ->
    #worklet_state{instances = Instances} = State,

    UpdatedInstances = lists:map(
        fun(I) when I#worklet_instance.instance_id =:= InstanceId ->
                I#worklet_instance{status = completed,
                                   completed_at = erlang:timestamp(),
                                   result = Result};
           (I) ->
                I
        end,
        Instances
    ),

    {reply, ok, State#worklet_state{instances = UpdatedInstances}};

handle_call({get_worklet_spec, SpecId}, _From, State) ->
    #worklet_state{specs = Specs} = State,

    case maps:get(SpecId, Specs, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Spec ->
            {reply, {ok, Spec}, State}
    end;

handle_call(list_worklet_specs, _From, State) ->
    #worklet_state{specs = Specs} = State,
    {reply, maps:values(Specs), State};

handle_call({set_worklet_parameter, SpecId, ParamName, _Value}, _From, State) ->
    #worklet_state{specs = Specs} = State,

    case maps:get(SpecId, Specs, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #worklet_spec{parameters = Params} = Spec ->
            ParamBinary = to_binary(ParamName),
            NewParams = case lists:member(ParamBinary, Params) of
                true -> Params;
                false -> [ParamBinary | Params]
            end,
            UpdatedSpec = Spec#worklet_spec{parameters = NewParams},
            {reply, ok, State#worklet_state{
                specs = Specs#{SpecId => UpdatedSpec}
            }}
    end;

handle_call({get_worklet_parameters, SpecId}, _From, State) ->
    #worklet_state{specs = Specs} = State,

    case maps:get(SpecId, Specs, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #worklet_spec{parameters = Params} ->
            {reply, {ok, Params}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

init(_Arg) ->
    process_flag(trap_exit, true),
    {ok, #worklet_state{}}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Safely checks a precondition function.
-spec check_precondition_safe(#worklet_spec{}, map()) -> boolean().
check_precondition_safe(#worklet_spec{pre_condition = undefined}, _Context) ->
    true;
check_precondition_safe(#worklet_spec{pre_condition = PreCondFun}, _Context)
  when is_function(PreCondFun, 0) ->
    try PreCondFun() of
        Result when is_boolean(Result) -> Result;
        _ -> false
    catch
        _:_ -> false
    end;
check_precondition_safe(#worklet_spec{pre_condition = PreCondFun}, Context)
  when is_function(PreCondFun, 1) ->
    try PreCondFun(Context) of
        Result when is_boolean(Result) -> Result;
        _ -> false
    catch
        _:_ -> false
    end;
check_precondition_safe(_, _) ->
    true.

%% @private
%% @doc Generates a unique identifier with a prefix.
-spec generate_id(binary()) -> binary().
generate_id(Prefix) ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<Prefix/binary, "_", Hex/binary>>.

%% @private
%% @doc Ensures all elements in the list are binaries.
-spec ensure_binary_list([binary() | atom() | string()]) -> [binary()].
ensure_binary_list(List) ->
    [to_binary(E) || E <- List].

%% @private
%% @doc Converts various types to binary.
-spec to_binary(binary() | atom() | string() | integer()) -> binary().
to_binary(B) when is_binary(B) -> B;
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(S) when is_list(S) -> list_to_binary(S);
to_binary(I) when is_integer(I) -> integer_to_binary(I).
