%% -*- erlang -*-
%% @doc Training Data Collection for Predictive Mining
%%
%% Collects and manages training data from event logs.
%%
%% @end

-module(pred_training).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([extract_sequences/1, extract_features/1]).
-export([build_training_set/2, split_train_test/3]).
-export([record_training_event/4, get_training_data/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Records
-record(state, {
    training_data :: map(),  %% workflow_id -> [training_example()]
    event_buffer :: queue:queue(),
    max_buffer_size :: pos_integer()
}).

-record(training_example, {
    input :: [float()],
    target :: float(),
    metadata :: map()
}).

-type training_example() :: #training_example{}.
-type split_mode() :: random | sequential.
-export_type([training_example/0, split_mode/0]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).

%% @doc Extract sequences from XES/OCEL logs.
-spec extract_sequences(map()) -> [[atom()]].
extract_sequences(EventLog) ->
    Cases = maps:get(cases, EventLog, #{}),
    lists:foldl(fun({CaseId, CaseData}, Acc) ->
        Events = maps:get(events, CaseData, []),
        Sequence = [maps:get(activity, E, undefined) || E <- Events],
        Acc ++ [Sequence]
    end, [], maps:to_list(Cases)).

%% @doc Extract features from events.
-spec extract_features(map()) -> [float()].
extract_features(Event) ->
    Activity = maps:get(activity, Event, <<>>),
    Timestamp = maps:get(timestamp, Event, 0),
    Resource = maps:get(resource, Event, <<>>),
    Duration = maps:get(duration, Event, 0),
    %% Convert to feature vector
    [
        activity_to_float(Activity),
        float(Timestamp),
        resource_to_float(Resource),
        float(Duration)
    ].

%% @doc Build training set with sliding window.
-spec build_training_set([[atom()]], pos_integer()) -> {[#training_example{}], map()}.
build_training_set(Sequences, WindowSize) ->
    Examples = lists:foldl(fun(Seq, Acc) ->
        build_examples_from_sequence(Seq, WindowSize, Acc)
    end, [], Sequences),
    Config = #{
        window_size => WindowSize,
        total_examples => length(Examples)
    },
    {Examples, Config}.

%% @doc Split data into train/test sets.
-spec split_train_test([#training_example{}], float(), binary()) ->
    {[#training_example{}], [#training_example{}]}.
split_train_test(Examples, TrainRatio, random) when is_float(TrainRatio), TrainRatio > 0, TrainRatio < 1 ->
    %% Shuffle and split
    Shuffled = shuffle_list(Examples),
    SplitPoint = round(length(Shuffled) * TrainRatio),
    lists:split(SplitPoint, Shuffled);
split_train_test(Examples, TrainRatio, sequential) ->
    SplitPoint = round(length(Examples) * TrainRatio),
    lists:split(SplitPoint, Examples).

%% @doc Record a training event from telemetry.
-spec record_training_event(binary(), term(), map(), integer()) -> ok.
record_training_event(WorkflowId, EventName, Labels, Timestamp) ->
    gen_server:cast(?SERVER, {record_event, WorkflowId, EventName, Labels, Timestamp}).

%% @doc Get training data for workflow.
-spec get_training_data(binary()) -> {ok, [#training_example{}]} | {error, not_found}.
get_training_data(WorkflowId) ->
    gen_server:call(?SERVER, {get_training_data, WorkflowId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{
        training_data = #{},
        event_buffer = queue:new(),
        max_buffer_size = 10000
    }}.

handle_call({get_training_data, WorkflowId}, _From, State) ->
    case maps:get(WorkflowId, State#state.training_data, undefined) of
        undefined -> {reply, {error, not_found}, State};
        Data -> {reply, {ok, Data}, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({record_event, WorkflowId, EventName, Labels, Timestamp}, State) ->
    %% Create a training example from the event
    Input = [
        activity_to_float(EventName),
        float(Timestamp),
        resource_to_float(maps:get(resource, Labels, <<>>)),
        float(maps:get(duration, Labels, 0))
    ],
    Target = case maps:get(outcome, Labels, undefined) of
        undefined -> 0.0;
        success -> 1.0;
        failure -> 0.0;
        Outcome when is_float(Outcome) -> Outcome;
        Outcome when is_integer(Outcome) -> float(Outcome)
    end,

    Example = #training_example{
        input = Input,
        target = Target,
        metadata = Labels#{
            workflow_id => WorkflowId,
            event_name => EventName,
            timestamp => Timestamp
        }
    },

    %% Add to existing training data or create new entry
    CurrentData = maps:get(WorkflowId, State#state.training_data, []),
    NewData = [Example | CurrentData],

    %% Manage buffer size - keep only most recent N examples per workflow
    TrimmedData = case length(NewData) > State#state.max_buffer_size of
        true -> lists:sublist(lists:reverse(NewData), State#state.max_buffer_size);
        false -> NewData
    end,

    NewTrainingData = maps:put(WorkflowId, TrimmedData, State#state.training_data),

    {noreply, State#state{training_data = NewTrainingData}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec build_examples_from_sequence([atom()], pos_integer(), [#training_example{}]) -> [#training_example{}].
build_examples_from_sequence(_Sequence, WindowSize, Acc) when length(_Sequence) < WindowSize ->
    Acc;
build_examples_from_sequence(Sequence, WindowSize, Acc) ->
    {Input, Rest} = lists:split(WindowSize, Sequence),
    %% Get next activity as target
    Target = case Rest of
        [] -> lists:last(Input);
        [Next | _] -> Next
    end,
    Example = #training_example{
        input = encode_activities(Input),
        target = activity_to_float(Target),
        metadata = #{window_size => WindowSize}
    },
    build_examples_from_sequence(tl(Sequence), WindowSize, [Example|Acc]).

%% @private
encode_activities(Activities) ->
    lists:map(fun activity_to_float/1, Activities).

%% @private
-spec activity_to_float(term()) -> float().
activity_to_float(undefined) -> 0.0;
activity_to_float(A) when is_atom(A) -> float(erlang:phash2(A));
activity_to_float(B) when is_binary(B) -> float(erlang:phash2(B));
activity_to_float(N) when is_integer(N) -> float(N);
activity_to_float(F) when is_float(F) -> F.

%% @private
resource_to_float(<<>>) -> 0.0;
resource_to_float(Resource) when is_binary(Resource) ->
    float(erlang:phash2(Resource));
resource_to_float(Resource) -> activity_to_float(Resource).

%% @private
shuffle_list(List) ->
    [V || {_, V} <- lists:sort(fun({A, _}, {B, _}) -> A =< B end,
        [{rand:uniform(), E} || E <- List])].
