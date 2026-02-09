# YAWL Pattern Usage Examples

This document provides practical implementation examples for all 43 YAWL patterns, including code snippets, workflow configurations, and usage scenarios.

---

## 1. Basic Control Flow Pattern Examples

### WCP-01: Sequence Pattern Example

```erlang
%% Order Processing Workflow - Linear Sequence
order_processing_workflow() ->
    Workflow = cre_yawl:new_workflow(<<"order_processing">>),

    %% Define sequence of tasks
    ValidateOrder = cre_yawl:add_task(Workflow, <<"validate_order">>,
                                     [{type, atomic}]),
    CheckInventory = cre_yawl:add_task(ValidateOrder, <<"check_inventory">>,
                                      [{type, atomic}]),
    ProcessPayment = cre_yawl:add_task(CheckInventory, <<"process_payment">>,
                                     [{type, atomic}]),
    ShipOrder = cre_yawl:add_task(ProcessPayment, <<"ship_order">>,
                                   [{type, atomic}]),
    ConfirmOrder = cre_yawl:add_task(ShipOrder, <<"confirm_order">>,
                                    [{type, atomic}]),

    %% Connect tasks in sequence
    W1 = cre_yawl:connect(ConfirmOrder, <<"validate_order">>, <<"check_inventory">>),
    W2 = cre_yawl:connect(W1, <<"check_inventory">>, <<"process_payment">>),
    W3 = cre_yawl:connect(W2, <<"process_payment">>, <<"ship_order">>),
    W4 = cre_yawl:connect(W3, <<"ship_order">>, <<"confirm_order">>),

    %% Set workflow boundaries
    FinalWorkflow = cre_yawl:set_workflow_boundaries(W4, <<"validate_order">>,
                                                   [<<"confirm_order">>]),

    %% Validate workflow
    case cre_yawl:validate(FinalWorkflow) of
        ok -> FinalWorkflow;
        {error, Errors} ->
            io:format("Validation errors: ~p~n", [Errors]),
            error(validation_failed)
    end.
```

**Usage Scenario**: Customer order processing where each step must complete before the next begins.

---

### WCP-02: Parallel Split Example

```erlang
%% Document Processing - Parallel Split
document_processing_workflow() ->
    Workflow = cre_yawl:new_workflow(<<"document_processing">>),

    %% Split task
    SplitTask = cre_yawl:add_task(Workflow, <<"start_processing">>,
                                  [{type, atomic}]),

    %% Parallel branches
    OCRBranch = cre_yawl:add_task(Workflow, <<"ocr_processing">>,
                                   [{type, atomic}]),
    ValidationBranch = cre_yawl:add_task(Workflow, <<"content_validation">>,
                                        [{type, atomic}]),
    IndexingBranch = cre_yawl:add_task(Workflow, <<"content_indexing">>,
                                      [{type, atomic}]),

    %% Join task
    JoinTask = cre_yawl:add_task(Workflow, <<"merge_results">>,
                                 [{type, atomic}]),

    %% Configure split as AND split
    Split = cre_yawl:set_split_type(Workflow, <<"start_processing">>, 'and_split'),

    %% Configure join as AND join
    Join = cre_yawl:set_join_type(Split, <<"merge_results">>, 'and_join'),

    %% Connect branches
    W1 = cre_yawl:connect(Join, <<"start_processing">>, <<"ocr_processing">>),
    W2 = cre_yawl:connect(W1, <<"start_processing">>, <<"content_validation">>),
    W3 = cre_yawl:connect(W2, <<"start_processing">>, <<"content_indexing">>),
    W4 = cre_yawl:connect(W3, <<"ocr_processing">>, <<"merge_results">>),
    W5 = cre_yawl:connect(W4, <<"content_validation">>, <<"merge_results">>),
    W6 = cre_yawl:connect(W5, <<"content_indexing">>, <<"merge_results">>),

    W6.
```

**Usage Scenario**: Simultaneous document processing where OCR, validation, and indexing can run in parallel.

---

### WCP-03: Synchronization Example

```erlang
%% Image Processing - Synchronization
image_processing_workflow() ->
    Workflow = cre_yawl:new_workflow(<<"image_processing">>),

    %% Pre-processing tasks (can run in parallel)
    ResizeTask = cre_yawl:add_task(Workflow, <<"resize_images">>,
                                   [{type, atomic}]),
    FilterTask = cre_yawl:add_task(Workflow, <<"apply_filters">>,
                                  [{type, atomic}]),
    CompressTask = cre_yawl:add_task(Workflow, <<"compress_images">>,
                                     [{type, atomic}]),

    %% Synchronization point
    SyncTask = cre_yawl:add_task(Workflow, <<"wait_for_preprocessing">>,
                                 [{type, atomic}]),

    %% Post-processing task (waits for all pre-processing to complete)
    WatermarkTask = cre_yawl:add_task(SyncTask, <<"apply_watermark">>,
                                     [{type, atomic}]),

    %% Configure synchronization
    Sync = cre_yawl:set_join_type(Workflow, <<"wait_for_preprocessing">>,
                                  'and_join'),

    %% Connect to synchronization point
    W1 = cre_yawl:connect(Sync, <<"resize_images">>, <<"wait_for_preprocessing">>),
    W2 = cre_yawl:connect(W1, <<"apply_filters">>, <<"wait_for_preprocessing">>),
    W3 = cre_yawl:connect(W2, <<"compress_images">>, <<"wait_for_preprocessing">>),
    W4 = cre_yawl:connect(W3, <<"wait_for_preprocessing">>, <<"apply_watermark">>),

    W4.
```

**Usage Scenario**: Image processing pipeline where multiple pre-processing steps must complete before applying watermarks.

---

### WCP-04: Exclusive Choice Example

```erlang
%% Customer Support Routing - Exclusive Choice
customer_support_workflow() ->
    Workflow = cre_yawl:new_workflow(<<"customer_support">>),

    %% Initial triage
    TriageTask = cre_yawl:add_task(Workflow, <<"triage_request">>,
                                  [{type, atomic}]),

    %% Possible routing options
    TechSupport = cre_yawl:add_task(Workflow, <<"technical_support">>,
                                    [{type, atomic}]),
    BillingSupport = cre_yawl:add_task(Workflow, <<"billing_support">>,
                                     [{type, atomic}]),
    GeneralSupport = cre_yawl:add_task(Workflow, <<"general_support">>,
                                     [{type, atomic}]),
    Escalation = cre_yawl:add_task(Workflow, <<"escalate_to_manager">>,
                                   [{type, atomic}]),

    %% Configure exclusive choice (XOR split)
    Split = cre_yawl:set_split_type(Workflow, <<"triage_request">>, 'xor_split'),

    %% Add routing conditions
    TechCondition = cre_yawl:add_condition(Workflow, <<"is_technical">>,
                                          fun() -> is_technical_issue() end),
    BillingCondition = cre_yawl:add_condition(Workflow, <<"is_billing">>,
                                           fun() -> is_billing_issue() end),
    GeneralCondition = cre_yawl:add_condition(Workflow, <<"is_general">>,
                                           fun() -> is_general_issue() end),
    EscalationCondition = cre_yawl:add_condition(Workflow, <<"needs_escalation">>,
                                                fun() -> needs_escalation() end),

    %% Connect with conditions
    W1 = cre_yawl:connect_with_condition(Split, <<"triage_request">>,
                                        <<"technical_support">>, <<"is_technical">>),
    W2 = cre_yawl:connect_with_condition(W1, <<"triage_request">>,
                                        <<"billing_support">>, <<"is_billing">>),
    W3 = cre_yawl:connect_with_condition(W2, <<"triage_request">>,
                                        <<"general_support">>, <<"is_general">>),
    W4 = cre_yawl:connect_with_condition(W3, <<"triage_request">>,
                                        <<"escalate_to_manager">>, <<"needs_escalation">>),

    W4.
```

**Usage Scenario**: Customer support ticket routing based on issue type.

---

### WCP-05: Simple Merge Example

```erlang
%% Order Fulfillment - Simple Merge
order_fulfillment_workflow() ->
    Workflow = cre_yawl:new_workflow(<<"order_fulfillment">>),

    %% Alternative paths to order completion
    InStorePickup = cre_yawl:add_task(Workflow, <<"in_store_pickup">>,
                                      [{type, atomic}]),
    HomeDelivery = cre_yawl:add_task(Workflow, <<"home_delivery">>,
                                   [{type, atomic}]),
    StoreDelivery = cre_yawl:add_task(Workflow, <<"store_delivery">>,
                                     [{type, atomic}]),

    %% Merge point
    MergeTask = cre_yawl:add_task(Workflow, <<"complete_order">>,
                                 [{type, atomic}]),

    %% Configure simple merge (XOR join - only one path taken)
    Merge = cre_yawl:set_join_type(Workflow, <<"complete_order">>,
                                   'xor_join'),

    %% Connect all paths to merge
    W1 = cre_yawl:connect(Merge, <<"in_store_pickup">>, <<"complete_order">>),
    W2 = cre_yawl:connect(W1, <<"home_delivery">>, <<"complete_order">>),
    W3 = cre_yawl:connect(W2, <<"store_delivery">>, <<"complete_order">>),

    W3.
```

**Usage Scenario**: Order fulfillment where only one delivery method is chosen.

---

### WCP-06: Multi-Choice Example

```erlang
%% Marketing Campaign - Multi-Choice
marketing_campaign_workflow() ->
    Workflow = cre_yawl:new_workflow(<<"marketing_campaign">>),

    %% Multi-split task
    SplitTask = cre_yawl:add_task(Workflow, <<"distribute_campaign">>,
                                  [{type, atomic}]),

    %% Multiple simultaneous channels
    EmailChannel = cre_yawl:add_task(Workflow, <<"email_campaign">>,
                                    [{type, atomic}]),
    SocialMediaChannel = cre_yawl:add_task(Workflow, <<"social_media_campaign">>,
                                          [{type, atomic}]),
    SMSChannel = cre_yawl:add_task(Workflow, <<"sms_campaign">>,
                                  [{type, atomic}]),
    PushNotificationChannel = cre_yawl:add_task(Workflow, <<"push_notification_campaign">>,
                                               [{type, atomic}]),

    %% Configure OR split (multiple channels can be active)
    Split = cre_yawl:set_split_type(Workflow, <<"distribute_campaign">>, 'or_split'),

    %% Add conditions for each channel
    EmailCondition = cre_yawl:add_condition(Workflow, <<"has_email_list">>,
                                          fun() -> has_email_list() end),
    SocialCondition = cre_yawl:add_condition(Workflow, <<"has_social_media">>,
                                            fun() -> has_social_media() end),
    SMSCondition = cre_yawl:add_condition(Workflow, <<"has_phone_numbers">>,
                                         fun() -> has_phone_numbers() end),
    PushCondition = cre_yawl:add_condition(Workflow, <<"has_app_users">>,
                                          fun() -> has_app_users() end),

    %% Connect channels with conditions
    W1 = cre_yawl:connect_with_condition(Split, <<"distribute_campaign">>,
                                        <<"email_campaign">>, <<"has_email_list">>),
    W2 = cre_yawl:connect_with_condition(W1, <<"distribute_campaign">>,
                                        <<"social_media_campaign">>, <<"has_social_media">>),
    W3 = cre_yawl:connect_with_condition(W2, <<"distribute_campaign">>,
                                        <<"sms_campaign">>, <<"has_phone_numbers">>),
    W4 = cre_yawl:connect_with_condition(W3, <<"distribute_campaign">>,
                                        <<"push_notification_campaign">>, <<"has_app_users">>),

    W4.
```

**Usage Scenario**: Multi-channel marketing campaign where available channels are activated based on available data.

---

## 2. Advanced Synchronization Pattern Examples

### WCP-07: Synchronizing Merge Example

```erlang
%% Data Analysis Pipeline - Synchronizing Merge
data_analysis_pipeline() ->
    Workflow = cre_yawl:new_workflow(<<"data_analysis">>),

    %% Data collection tasks (parallel)
    CollectSensorData = cre_yawl:add_task(Workflow, <<"collect_sensor_data">>,
                                         [{type, atomic}]),
    CollectUserInput = cre_yawl:add_task(Workflow, <<"collect_user_input">>,
                                       [{type, atomic}]),
    CollectExternalData = cre_yawl:add_task(Workflow, <<"collect_external_data">>,
                                          [{type, atomic}]),

    %% Analysis tasks (can run as data becomes available)
    AnalyzeSensorData = cre_yawl:add_task(Workflow, <<"analyze_sensor_data">>,
                                        [{type, atomic}]),
    AnalyzeUserData = cre_yawl:add_task(Workflow, <<"analyze_user_data">>,
                                      [{type, atomic}]),
    AnalyzeExternalData = cre_yawl:add_task(Workflow, <<"analyze_external_data">>,
                                           [{type, atomic}]),

    %% Synchronizing merge point
    SyncMergeTask = cre_yawl:add_task(Workflow, <<"merge_analysis_results">>,
                                     [{type, atomic}]),

    %% Configure synchronizing merge
    SyncMerge = cre_yawl:set_join_type(Workflow, <<"merge_analysis_results">>,
                                      'synchronizing_merge'),

    %% Connect collection to analysis
    W1 = cre_yawl:connect(SyncMerge, <<"collect_sensor_data">>,
                          <<"analyze_sensor_data">>),
    W2 = cre_yawl:connect(W1, <<"collect_user_input">>,
                          <<"analyze_user_data">>),
    W3 = cre_yawl:connect(W2, <<"collect_external_data">>,
                          <<"analyze_external_data">>),

    %% Connect analysis to merge point
    W4 = cre_yawl:connect(W3, <<"analyze_sensor_data">>,
                          <<"merge_analysis_results">>),
    W5 = cre_yawl:connect(W4, <<"analyze_user_data">>,
                          <<"merge_analysis_results">>),
    W6 = cre_yawl:connect(W5, <<"analyze_external_data">>,
                          <<"merge_analysis_results">>),

    W6.
```

**Usage Scenario**: Data analysis where results from different sources are merged as they become available, but all are required for final analysis.

---

### WCP-08: Multi-Merge Example

```erlang
%% IoT Data Processing - Multi-Merge
iot_data_processing_workflow() ->
    Workflow = cre_yawl:new_workflow(<<"iot_data_processing">>),

    %% Multiple data sources
    TemperatureSensor = cre_yawl:add_task(Workflow, <<"temperature_data">>,
                                        [{type, atomic}]),
    HumiditySensor = cre_yawl:add_task(Workflow, <<"humidity_data">>,
                                      [{type, atomic}]),
    PressureSensor = cre_yawl:add_task(Workflow, <<"pressure_data">>,
                                      [{type, atomic}]),
    MotionSensor = cre_yawl:add_task(Workflow, <<"motion_data">>,
                                    [{type, atomic}]),

    %% Processing tasks for each sensor type
    ProcessTemperature = cre_yawl:add_task(Workflow, <<"process_temperature">>,
                                          [{type, atomic}]),
    ProcessHumidity = cre_yawl:add_task(Workflow, <<"process_humidity">>,
                                       [{type, atomic}]),
    ProcessPressure = cre_yawl:add_task(Workflow, <<"process_pressure">>,
                                       [{type, atomic}]),
    ProcessMotion = cre_yawl:add_task(Workflow, <<"process_motion">>,
                                     [{type, atomic}]),

    %% Multi-merge task collects all processed data
    DataFusionTask = cre_yawl:add_task(Workflow, <<"data_fusion">>,
                                      [{type, atomic}]),

    %% Configure multi-merge
    DataFusion = cre_yawl:set_join_type(Workflow, <<"data_fusion">>,
                                       'multi_merge'),

    %% Connect sensors to processors
    W1 = cre_yawl:connect(DataFusion, <<"temperature_data">>,
                          <<"process_temperature">>),
    W2 = cre_yawl:connect(W1, <<"humidity_data">>,
                          <<"process_humidity">>),
    W3 = cre_yawl:connect(W2, <<"pressure_data">>,
                          <<"process_pressure">>),
    W4 = cre_yawl:connect(W3, <<"motion_data">>,
                          <<"process_motion">>),

    %% Connect processors to multi-merge
    W5 = cre_yawl:connect(W4, <<"process_temperature">>,
                          <<"data_fusion">>),
    W6 = cre_yawl:connect(W5, <<"process_humidity">>,
                          <<"data_fusion">>),
    W7 = cre_yawl:connect(W6, <<"process_pressure">>,
                          <<"data_fusion">>),
    W8 = cre_yawl:connect(W7, <<"process_motion">>,
                          <<"data_fusion">>),

    W8.
```

**Usage Scenario**: IoT data processing where data from multiple sensors is processed independently and then fused together.

---

## 3. Multiple Instance Pattern Examples

### WCP-11: Implicit Termination Example

```erlang
%% Batch Processing - Implicit Termination
batch_processing_workflow() ->
    %% Define batch processing function
    ProcessItem = fun(Item) ->
        %% Process individual item in batch
        timer:sleep(100),  % Simulate processing time
        case Item of
            {data, Value} when Value > 1000 -> {error, threshold_exceeded};
            {data, Value} -> {ok, processed(Value)};
            _ -> {ok, ignored}
        end
    end.

    %% Create implicit termination pattern
    Pattern = cre_yawl_patterns:implicit_termination(ProcessItem),

    %% Initialize with batch data
    BatchData = [
        {data, 100}, {data, 200}, {data, 1500},
        {data, 300}, {data, 400}
    ],
    PatternState = Pattern#pattern_state{
        pending_instances = BatchData,
        instance_count = 0
    },

    %% Process the batch
    process_batch(PatternState).
```

**Usage Scenario**: Batch processing where items are processed independently and the workflow terminates automatically when all items are processed.

---

### WCP-12: Multiple Instances without Synchronization Example

```erlang
%% Image Processing Pipeline - No Synchronization
image_processing_pipeline() ->
    %% Define image processing function
    ProcessImage = fun(ImageData) ->
        %% Process single image
        case ImageData of
            {image, File, Format} ->
                %% Simulate processing
                timer:sleep(random:uniform(2000)),  % Variable processing time
                {processed_image, File, enhanced(Format)};
            _ -> {error, invalid_format}
        end
    end.

    %% Create multiple instances without synchronization
    Pattern = cre_yawl_patterns:multiple_instances_no_sync(
        ProcessImage,    % Processing function
        5,               % Number of instances
        [                % Input data
            {image, "img1.jpg", jpg},
            {image, "img2.png", png},
            {image, "img3.gif", gif},
            {image, "img4.jpg", jpg},
            {image, "img5.png", png}
        ]
    ).

    %% Execute parallel processing
    execute_parallel(Pattern).
```

**Usage Scenario**: Parallel image processing where images are processed independently without waiting for others to complete.

---

### WCP-13: Multiple Instances with Design Time Knowledge Example

```erlang
%% Data Parallel Processing - Fixed Instances
data_parallel_processing() ->
    %% Define data processing function
    ProcessDataChunk = fun({chunk, Id, Data}) ->
        %% Process data chunk
        Results = lists:map(fun process_item/1, Data),
        {chunk_result, Id, Results}
    end.

    %% Create fixed number of instances
    InputData = [
        {chunk, 1, [d1, d2, d3, d4, d5]},
        {chunk, 2, [d6, d7, d8, d9, d10]},
        {chunk, 3, [d11, d12, d13, d14, d15]},
        {chunk, 4, [d16, d17, d18, d19, d20]}
    ],

    Pattern = cre_yawl_patterns:multiple_instances_static(
        ProcessDataChunk, % Processing function
        4,                % Fixed number of instances
        InputData         % Input data chunks
    ).

    %% Process with synchronization
    process_with_synchronization(Pattern).
```

**Usage Scenario**: Data parallelism where the number of parallel processes is known at design time and all must complete before proceeding.

---

### WCP-14: Multiple Instances with Runtime Knowledge Example

```erlang
%% Adaptive Processing - Runtime Instance Count
adaptive_processing_workflow() ->
    %% Define instance count determination function
    DetermineInstanceCount = fun(InputData) ->
        case InputData of
            {dataset, Items} when length(Items) > 1000 ->
                %% Large dataset: use more instances
                min(8, ceil(length(Items) / 125));
            {dataset, Items} when length(Items) > 100 ->
                %% Medium dataset: moderate instances
                min(4, ceil(length(Items) / 25));
            {dataset, Items} ->
                %% Small dataset: fewer instances
                min(2, length(Items));
            _ -> 1  % Default: single instance
        end
    end.

    %% Define processing function
    ProcessDatasetItem = fun(Item) ->
        %% Process individual item
        timer:sleep(50),  % Simulate processing
        {processed, Item}
    end.

    %% Create runtime-determined instances
    LargeDataset = lists:seq(1, 150),  % 150 items to process
    Pattern = cre_yawl_patterns:multiple_instances_runtime(
        ProcessDatasetItem, % Processing function
        DetermineInstanceCount, % Count determination
        {dataset, LargeDataset}  % Input data
    ).

    %% Process adaptively
    process_adaptively(Pattern).
```

**Usage Scenario**: Adaptive parallel processing where the number of parallel instances is determined based on dataset characteristics at runtime.

---

### WCP-15: Multiple Instances without Prior Knowledge Example

```erlang
%% Streaming Data Processing - Dynamic Instances
streaming_data_workflow() ->
    %% Define data stream generator
    DataStream = fun() ->
        %% Simulate streaming data
        receive
            {data, Items} ->
                case Items of
                    [] -> done;
                    _ -> {more, Items}
                end
        after 5000 ->
            timeout  % Timeout after 5 seconds
        end
    end.

    %% Define stream processing function
    ProcessStreamItem = fun(Item) ->
        %% Process streaming item
        case Item of
            {sensor_reading, SensorId, Value, Timestamp} ->
                Processed = normalize_sensor_data(SensorId, Value),
                {processed_reading, SensorId, Processed, Timestamp};
            {event, EventType, Data} ->
                {processed_event, EventType, Data};
            _ -> {ignored, Item}
        end
    end.

    %% Create dynamic instances from streaming data
    Pattern = cre_yawl_patterns:multiple_instances_dynamic(
        ProcessStreamItem, % Processing function
        DataStream,       % Data streaming function
        initial_data      % Initial data
    ).

    %% Process streaming data
    process_stream(Pattern).
```

**Usage Scenario**: Real-time streaming data processing where instances are created dynamically as data arrives.

---

## 4. State-Based Pattern Examples

### WCP-18: Milestone Example

```erlang
%% Project Management - Milestone-Based Progress
project_management_workflow() ->
    %% Define milestone check function
    UserStoryMilestoneReached = fun() ->
        case get(project_progress) of
            {progress, stories} when StoriesCompleted >= UserStoriesTarget -> true;
            {progress, phase} when Phase >= development -> true;
            _ -> false
        end
    end.

    %% Define user story processing activity
    ProcessUserStory = fun(Story) ->
        %% Process user story with milestone guard
        UserStoriesCompleted = UserStoriesCompleted + 1,
        set_project_progress({progress, stories, UserStoriesCompleted}),
        {processed_story, Story}
    end.

    %% Create milestone pattern
    Pattern = cre_yawl_patterns:milestone(
        ProcessUserStory,     % Activity to guard
        UserStoryMilestoneReached  % Milestone check
    ),

    %% Execute with milestone guard
    execute_with_milestone(Pattern).
```

**Usage Scenario**: Project management where user stories can only be processed after reaching certain milestones.

---

### WCP-19: Cancel Activity Example

```erlang
%% Long-Running Process with Cancellation
cancellable_long_process() ->
    %% Define cancellation condition
    ShouldCancelProcess = fun(ProcessId) ->
        case get(process_status) of
            {cancelled, ProcessId} -> true;
            {timeout, ProcessId} -> true;
            {error, ProcessId, Reason} when Reason =:= resource_unavailable -> true;
            _ -> false
        end
    end.

    %% Define long-running process
    LongRunningProcess = fun(Data) ->
        %% Process that may take a long time
        case ShouldCancelProcess(get(process_id)) of
            true -> {cancelled, get(process_id)};
            false ->
                %% Perform actual processing
                Result = perform_long_processing(Data),
                set_process_status({completed, get(process_id)}),
                {ok, Result}
        end
    end.

    %% Create cancellable activity
    Pattern = cre_yawl_patterns:cancel_activity(
        LongRunningProcess,  % Activity to cancel
        ShouldCancelProcess   % Cancellation condition
    ),

    %% Execute with potential cancellation
    execute_cancellable(Pattern).
```

**Usage Scenario**: Long-running operations that can be cancelled based on external conditions or timeouts.

---

## 5. Extended Control Flow Pattern Examples

### WCP-21: Structured Synchronization Example

```erlang
%% Collaborative Document Editing - Structured Sync
collaborative_editing_workflow() ->
    Workflow = cre_yawl:new_workflow(<<"collaborative_edit">>),

    %% Multiple editors working in parallel
    Editor1 = cre_yawl:add_task(Workflow, <<"editor1_chapter1">>,
                               [{type, atomic}]),
    Editor2 = cre_yawl:add_task(Workflow, <<"editor2_chapter2">>,
                               [{type, atomic}]),
    Editor3 = cre_yawl:add_task(Workflow, <<"editor3_chapter3">>,
                               [{type, atomic}]),

    %% Synchronization point for review
    ReviewSync = cre_yawl:add_task(Workflow, <<"content_review">>,
                                  [{type, atomic}]),

    %% Synchronized editing section
    SynchronizedEdit = cre_yawl:add_task(ReviewSync, <<"final_sync_edit">>,
                                        [{type, atomic}]),

    %% Configure structured synchronization
    Sync = cre_yawl:structured_sync(
        [Editor1, Editor2, Editor3],  % Activities to synchronize
        undefined  % Initial data
    ),

    %% Execute with structured synchronization
    execute_structured_sync(Sync).
```

**Usage Scenario**: Collaborative document editing where multiple editors work in parallel but must synchronize at review points.

---

### WCP-26: Critical Section Example

```erlang
%% Database Update with Critical Section
database_update_workflow() ->
    %% Define critical database operation
    CriticalDatabaseUpdate = fun(Data) ->
        %% Ensure exclusive access to database
        acquire_database_lock(),
        try
            %% Perform critical update
            Result = execute_database_transaction(Data),
            release_database_lock(),
            {ok, Result}
        catch
            Error:Reason ->
                release_database_lock(),
                {error, {Error, Reason}}
        end
    end.

    %% Create critical section pattern
    Pattern = cre_yawl_patterns:critical_section(
        CriticalDatabaseUpdate,  % Critical activity
        database_update_lock     % Lock identifier
    ),

    %% Execute with critical section protection
    execute_critical_section(Pattern).
```

**Usage Scenario**: Database operations requiring exclusive access to prevent concurrent modifications that could cause data corruption.

---

## 6. Data Flow Pattern Examples

### WDP-01: Parameter Passing Example

```erlang
%% Order Processing with Data Flow
order_processing_with_data_flow() ->
    Workflow = cre_yawl:new_workflow(<<"order_data_flow">>),

    %% Define tasks
    ValidateOrder = cre_yawl:add_task(Workflow, <<"validate_order">>,
                                      [{type, atomic}]),
    CalculateShipping = cre_yawl:add_task(Workflow, <<"calculate_shipping">>,
                                        [{type, atomic}]),
    ProcessPayment = cre_yawl:add_task(Workflow, <<"process_payment">>,
                                     [{type, atomic}]),
    FulfillOrder = cre_yawl:add_task(Workflow, <<"fulfill_order">>,
                                    [{type, atomic}]),

    %% Define parameter passing between tasks
    OrderData = cre_yawl:param_pass(<<"validate_order">>, <<"calculate_shipping">>),
    ShippingInfo = cre_yawl:param_pass(<<"calculate_shipping">>,
                                       <<"process_payment">>),
    PaymentInfo = cre_yawl:param_pass(<<"process_payment">>,
                                     <<"fulfill_order">>),

    %% Add patterns to workflow
    W1 = cre_yawl:add_pattern(Workflow, OrderData),
    W2 = cre_yawl:add_pattern(W1, ShippingInfo),
    W3 = cre_yawl:add_pattern(W2, PaymentInfo),

    %% Connect tasks
    W4 = cre_yawl:connect(W3, <<"validate_order">>, <<"calculate_shipping">>),
    W5 = cre_yawl:connect(W4, <<"calculate_shipping">>, <<"process_payment">>),
    W6 = cre_yawl:connect(W5, <<"process_payment">>, <<"fulfill_order">>),

    W6.
```

**Usage Scenario**: Order processing where data flows between tasks through parameter passing.

---

### WDP-02: Data Transformation Example

```erlang
%% Data Processing Pipeline with Transformation
data_transformation_workflow() ->
    Workflow = cre_yawl:new_workflow(<<"data_transform">>),

    %% Define transformation functions
    JsonToXmlTransform = fun(JsonData) ->
        %% Transform JSON to XML
        XmlData = json_to_xml_converter(JsonData),
        {transformed, xml, XmlData}
    end.

    DataValidationTransform = fun(Input) ->
        %% Validate and normalize data
        case Input of
            {raw, Data} when is_binary(Data) ->
                Validated = validate_data(Data),
                {validated, Validated};
            _ -> {error, invalid_format}
        end
    end.

    %% Define tasks
    RawDataTask = cre_yawl:add_task(Workflow, <<"raw_data_input">>,
                                   [{type, atomic}]),
    ValidateTask = cre_yawl:add_task(Workflow, <<"data_validation">>,
                                    [{type, atomic}]),
    TransformTask = cre_yawl:add_task(Workflow, <<"format_transformation">>,
                                     [{type, atomic}]),

    %% Define data transformation patterns
    ValidationTransform = cre_yawl:data_transform(
        <<"raw_data_input">>,
        <<"data_validation">>,
        DataValidationTransform,
        #{status => validated, data => binary()}  % Output schema
    ),

    FormatTransform = cre_yawl:data_transform(
        <<"data_validation">>,
        <<"format_transformation">>,
        JsonToXmlTransform,
        #{status => transformed, format => xml, data => binary()}  % Output schema
    ),

    %% Add patterns to workflow
    W1 = cre_yawl:add_pattern(Workflow, ValidationTransform),
    W2 = cre_yawl:add_pattern(W1, FormatTransform),

    %% Connect tasks
    W3 = cre_yawl:connect(W2, <<"raw_data_input">>, <<"data_validation">>),
    W4 = cre_yawl:connect(W3, <<"data_validation">>,
                         <<"format_transformation">>),

    W4.
```

**Usage Scenario**: Data processing pipeline where data is validated and transformed between processing stages.

---

## 7. Exception Handling Pattern Examples

### WHP-01: Error Handler Example

```erlang
%% File Processing with Error Handling
file_processing_with_error_handler() ->
    %% Define protected file operation
    ProcessFile = fun(File) ->
        %% Risky file operation
        case file:read_file(File) of
            {ok, Content} ->
                Processed = process_file_content(Content),
                {ok, Processed};
            {error, Reason} ->
                {error, {file_read_error, Reason}}
        end
    end.

    %% Define error handler
    ErrorHandler = fun(Error) ->
        case Error of
            {file_read_error, enoent} ->
                %% File not found - create placeholder
                {recovery, create_placeholder_file()};
            {file_read_error, eacces} ->
                %% Permission denied - log and skip
                log_error("Permission denied: " ++ ?FILE),
                {skip, Error};
            {file_read_error, Reason} ->
                %% Other errors - retry once
                {retry, Reason}
        end
    end.

    %% Create error handler pattern
    Pattern = cre_yawl_patterns:error_handler(
        ProcessFile,     % Protected activity
        ErrorHandler     % Error handler function
    ),

    %% Execute with error handling
    execute_with_error_handler(Pattern).
```

**Usage Scenario**: File processing operations that may fail and require specific error handling and recovery logic.

---

### WHP-02: Retry Example

```erlang
%% External Service Call with Retry
external_service_with_retry() ->
    %% Define external service call
    CallExternalService = fun(Request) ->
        %% Call external API
        case httpc:request(post, {ServiceUrl, [], "application/json",
                                 jsx:encode(Request)}, [], []) of
            {ok, {{_, 200, _}, _, Response}} ->
                {ok, jsx:decode(Response)};
            {ok, {{_, 429, _}, _, _}} = Response ->
                {rate_limited, Response};
            {ok, {{_, Status, _}, _, _}} = Response ->
                {error, {api_error, Status}};
            {error, Reason} ->
                {error, {network_error, Reason}}
        end
    end.

    %% Define backoff function
    ExponentialBackoff = fun(Attempt) ->
        %% Exponential backoff with jitter
        BaseDelay = 1000,  % 1 second base delay
        MaxDelay = 30000,  % 30 seconds max delay
        Delay = trunc(BaseDelay * math:pow(2, Attempt)),
        Jitter = random:uniform(1000),
        DelayWithJitter = Delay + Jitter,
        min(DelayWithJitter, MaxDelay)
    end.

    %% Create retry pattern
    Pattern = cre_yawl_patterns:retry(
        CallExternalService,  % Activity to retry
        3,                    % Maximum retries
        ExponentialBackoff    % Backoff function
    ),

    %% Execute with retry logic
    execute_with_retry(Pattern).
```

**Usage Scenario**: External service calls that may fail due to temporary issues and benefit from automatic retries with backoff.

---

### WHP-03: Compensation Example

```erlang
%% Transaction Processing with Compensation
transaction_processing_with_compensation() ->
    %% Define compensable activities
    UpdateDatabase = fun(Transaction) ->
        %% Update database with transaction
        case execute_update(Transaction) of
            {ok, Result} ->
                %% Register compensation function
                register_compensation(fun undo_transaction/1, Transaction),
                {ok, Result};
            {error, Reason} ->
                {error, Reason}
        end
    end.

    SendConfirmation = fun(Transaction) ->
        %% Send confirmation email
        case send_email(Transaction) of
            {ok, EmailId} ->
                %% Register compensation function
                register_compensation(fun cancel_email/1, EmailId),
                {ok, EmailId};
            {error, Reason} ->
                {error, Reason}
        end
    end.

    UpdateInventory = fun(Transaction) ->
        %% Update inventory
        case update_stock(Transaction) of
            {ok, Result} ->
                %% Register compensation function
                register_compensation(fun restore_inventory/1, Transaction),
                {ok, Result};
            {error, Reason} ->
                {error, Reason}
        end
    end.

    %% Define compensation functions
    UndoTransaction = fun(Transaction) ->
        %% Undo database changes
        execute_rollback(Transaction)
    end.

    CancelEmail = fun(EmailId) ->
        %% Cancel sent email
        cancel_email_by_id(EmailId)
    end.

    RestoreInventory = fun(Transaction) ->
        %% Restore inventory levels
        restore_stock_levels(Transaction)
    end.

    %% Create compensation patterns for each activity
    DatabaseCompensate = cre_yawl_patterns:compensate(
        UpdateDatabase,    % Compensable activity
        UndoTransaction    % Compensator function
    ),

    EmailCompensate = cre_yawl_patterns:compensate(
        SendConfirmation,  % Compensable activity
        CancelEmail        % Compensator function
    ),

    InventoryCompensate = cre_yawl_patterns:compensate(
        UpdateInventory,   % Compensable activity
        RestoreInventory   % Compensator function
    ),

    %% Execute with compensation support
    execute_with_compensation([DatabaseCompensate, EmailCompensate, InventoryCompensate]).
```

**Usage Scenario**: Transaction processing where operations must be undone if any part of the transaction fails.

---

### WHP-04: Triggered Compensation Example

```erlang
%% E-commerce Order with Triggered Compensation
ecommerce_order_with_triggered_compensation() ->
    %% Define order processing activities
    ProcessPayment = fun(Order) ->
        %% Process payment
        case payment_gateway:process(Order#order.payment) of
            {success, TransactionId} ->
                {ok, TransactionId};
            {failed, Reason} ->
                {error, {payment_failed, Reason}}
        end
    end.

    ReserveInventory = fun(Order) ->
        %% Reserve inventory items
        case inventory:reserve(Order#order.items) of
            {ok, ReservationId} ->
                {ok, ReservationId};
            {error, Reason} ->
                {error, {inventory_failed, Reason}}
        end
    end.

    ShipOrder = fun(Order) ->
        %% Ship order
        case shipping:ship(Order) of
            {ok, TrackingId} ->
                {ok, TrackingId};
            {error, Reason} ->
                {error, {shipping_failed, Reason}}
        end
    end.

    %% Define trigger condition for compensation
    ShouldTriggerCompensation = fun(Event) ->
        case Event of
            {payment_failed, Reason} -> true;
            {inventory_failed, Reason} -> true;
            {shipping_failed, Reason} -> true;
            {quality_check_failed} -> true;
            _ -> false
        end
    end.

    %% Define compensation functions
    RefundPayment = fun(Order) ->
        payment_gateway:refund(Order#order.payment)
    end.

    ReleaseInventory = fun(Order) ->
        inventory:release(Order#order.items)
    end.

    CancelShipment = fun(Order) ->
        shipping:cancel(Order)
    end.

    %% Create triggered compensation patterns
    PaymentCompensation = cre_yawl_patterns:triggered_compensation(
        ProcessPayment,       % Activity
        RefundPayment,         % Compensator
        ShouldTriggerCompensation  % Trigger condition
    ),

    InventoryCompensation = cre_yawl_patterns:triggered_compensation(
        ReserveInventory,     % Activity
        ReleaseInventory,     % Compensator
        ShouldTriggerCompensation  % Trigger condition
    ),

    ShippingCompensation = cre_yawl_patterns:triggered_compensation(
        ShipOrder,            % Activity
        CancelShipment,       % Compensator
        ShouldTriggerCompensation  % Trigger condition
    ),

    %% Execute with triggered compensation
    execute_triggered_compensation([PaymentCompensation, InventoryCompensation,
                                  ShippingCompensation]).
```

**Usage Scenario**: E-commerce order processing where compensation is triggered based on specific failure conditions.

---

### WHP-05: Consecutive Compensation Example

```erlang
%% Multi-Step Process with Consecutive Compensation
multi_step_process_with_compensation() ->
    %% Define activities and their compensators
    ActivityCompensatorPairs = [
        {fun authenticate_user/1,               % Authentication
         fun deauthenticate_user/1},           % De-authentication
        {fun reserve_resources/1,              % Resource reservation
         fun release_resources/1},           % Resource release
        {fun start_transaction/1,             % Transaction start
         fun rollback_transaction/1},         % Transaction rollback
        {fun apply_changes/1,                 % Apply changes
         fun revert_changes/1},               % Revert changes
        {fun notify_stakeholders/1,           % Notification
         fun cancel_notifications/1}         % Cancel notifications
    ],

    %% Create consecutive compensation pattern
    CompensationChain = cre_yawl_patterns:consecutive_compensate(
        ActivityCompensatorPairs
    ),

    %% Execute with consecutive compensation
    execute_with_consecutive_compensation(CompensationChain).
```

**Usage Scenario**: Complex multi-step processes where operations must be undone in reverse order when failures occur.

---

## Pattern Composition Examples

### Composition: Parallel Split + Multiple Instances + Synchronization

```erlang
%% Batch Processing with Parallel Execution and Synchronization
batch_processing_composition() ->
    Workflow = cre_yawl:new_workflow(<<"batch_parallel">>),

    %% Split task
    SplitTask = cre_yawl:add_task(Workflow, <<"start_batch_processing">>,
                                  [{type, atomic}]),

    %% Multiple instance processing branches
    BatchBranch1 = cre_yawl:add_task(Workflow, <<"process_type_a">>,
                                    [{type, atomic}]),
    BatchBranch2 = cre_yawl:add_task(Workflow, <<"process_type_b">>,
                                    [{type, atomic}]),
    BatchBranch3 = cre_yawl:add_task(Workflow, <<"process_type_c">>,
                                    [{type, atomic}]),

    %% Synchronization point
    SyncTask = cre_yawl:add_task(Workflow, <<"wait_for_batches">>,
                                 [{type, atomic}]),

    %% Configure parallel split
    Split = cre_yawl:set_split_type(Workflow, <<"start_batch_processing">>,
                                   'and_split'),

    %% Configure multiple instances for each branch
    InstancePattern1 = cre_yawl_patterns:multiple_instances_static(
        fun process_type_a_item/1,  % Processing function
        5,                          % Number of instances
        get_type_a_items()          % Input data
    ),

    InstancePattern2 = cre_yawl_patterns:multiple_instances_static(
        fun process_type_b_item/1,  % Processing function
        3,                          % Number of instances
        get_type_b_items()          % Input data
    ),

    InstancePattern3 = cre_yawl_patterns:multiple_instances_static(
        fun process_type_c_item/1,  % Processing function
        4,                          % Number of instances
        get_type_c_items()          % Input data
    ),

    %% Configure synchronization
    Sync = cre_yawl:set_join_type(Workflow, <<"wait_for_batches">>,
                                 'and_join'),

    %% Connect branches
    W1 = cre_yawl:connect(Sync, <<"start_batch_processing">>,
                          <<"process_type_a">>),
    W2 = cre_yawl:connect(W1, <<"start_batch_processing">>,
                          <<"process_type_b">>),
    W3 = cre_yawl:connect(W2, <<"start_batch_processing">>,
                          <<"process_type_c">>),
    W4 = cre_yawl:connect(W3, <<"process_type_a">>,
                          <<"wait_for_batches">>),
    W5 = cre_yawl:connect(W4, <<"process_type_b">>,
                          <<"wait_for_batches">>),
    W6 = cre_yawl:connect(W5, <<"process_type_c">>,
                          <<"wait_for_batches">>),

    %% Add instance patterns
    W7 = cre_yawl:add_pattern(W6, InstancePattern1),
    W8 = cre_yawl:add_pattern(W7, InstancePattern2),
    W9 = cre_yawl:add_pattern(W8, InstancePattern3),

    W9.
```

**Usage Scenario**: Large batch processing where different data types are processed in parallel with multiple instances per type.

---

### Composition: Exclusive Choice + Error Handling + Compensation

```erlang
%% Order Processing with Advanced Error Handling
order_processing_advanced() ->
    %% Define order routing
    RouteOrder = fun(Order) ->
        case Order#order.type of
            standard -> standard_order_workflow(Order);
            express -> express_order_workflow(Order);
            international -> international_order_workflow(Order);
            _ -> error(unknown_order_type)
        end
    end.

    %% Define standard order workflow
    StandardOrder = fun(Order) ->
        %% Process standard order
        case validate_order(Order) of
            {ok, Validated} ->
                case process_payment(Validated) of
                    {ok, Processed} ->
                        case ship_order(Processed) of
                            {ok, Shipped} -> {success, Shipped};
                            {error, Reason} -> {error, shipping_failed, Reason}
                        end;
                    {error, Reason} -> {error, payment_failed, Reason}
                end;
            {error, Reason} -> {error, validation_failed, Reason}
        end
    end.

    %% Define error handlers
    PaymentErrorHandler = fun(Error) ->
        case Error of
            {payment_failed, Reason} -> handle_payment_error(Reason);
            {shipping_failed, Reason} -> handle_shipping_error(Reason);
            {validation_failed, Reason} -> handle_validation_error(Reason);
            _ -> handle_generic_error(Error)
        end
    end.

    %% Define compensation
    OrderCompensation = fun(Order) ->
        %% Undo order processing
        cancel_order(Order)
    end.

    %% Create pattern composition
    RoutePattern = cre_yawl_patterns:exclusive_choice(
        #{standard => fun standard_order/1,
          express => fun express_order/1,
          international => fun international_order/1},
        fun(Order) -> Order#order.type end,  % Route decision function
        undefined  % Initial data
    ),

    ErrorHandlingPattern = cre_yawl_patterns:error_handler(
        StandardOrder,    % Protected activity
        PaymentErrorHandler  % Error handler
    ),

    CompensationPattern = cre_yawl_patterns:compensate(
        RouteOrder,        % Compensable activity
        OrderCompensation  % Compensator function
    ),

    %% Execute composed pattern
    execute_composed_patterns([RoutePattern, ErrorHandlingPattern, CompensationPattern]).
```

**Usage Scenario**: Complex order processing with multiple order types, comprehensive error handling, and compensation capabilities.

---

## Performance Optimization Examples

### Optimized Parallel Processing

```erlang
%% Optimized Multiple Instance Processing
optimized_parallel_processing() ->
    %% Define workload-aware instance creation
    CreateOptimalInstances = fun(Data) ->
        %% Determine optimal instance count based on system load
        SystemLoad = get_system_load(),
        DataSize = get_data_size(Data),

        OptimalCount = case {SystemLoad, DataSize} of
            {low, _} -> min(8, ceil(DataSize / optimal_chunk_size()));
            {medium, _} -> min(4, ceil(DataSize / optimal_chunk_size()));
            {high, _} -> min(2, ceil(DataSize / optimal_chunk_size()));
            _ -> 1
        end,

        %% Create instances with balanced workload
        cre_yawl_patterns:multiple_instances_static(
            fun process_chunk/1,  % Processing function
            OptimalCount,        % Optimized instance count
            distribute_data(Data, OptimalCount)  % Balanced data distribution
        )
    end.

    %% Define efficient processing function
    ProcessChunk = fun({chunk, Id, Data}) ->
        %% Process data chunk efficiently
        Results = lists:map(fun optimized_process_item/1, Data),
        {chunk_result, Id, Results}
    end.

    %% Execute with optimization
    execute_optimized(CreateOptimalInstances).
```

**Usage Scenario**: Parallel processing that adapts instance counts based on system load and data characteristics for optimal performance.

---

## Best Practices Summary

1. **Start Simple**: Begin with basic patterns and add complexity only when needed
2. **Monitor Performance**: Track execution metrics for complex patterns
3. **Handle Errors Gracefully**: Implement proper error handling and compensation
4. **Use Appropriate Synchronization**: Avoid unnecessary synchronization points
5. **Consider Resource Usage**: Monitor memory and CPU usage for parallel patterns
6. **Document Patterns**: Clearly document pattern usage and intentions
7. **Test Thoroughly**: Test edge cases and failure scenarios
8. **Optimize Incrementally**: Profile and optimize performance bottlenecks

---

These examples demonstrate practical implementations of all 43 YAWL patterns in various scenarios. Each pattern can be combined and customized to fit specific workflow requirements.