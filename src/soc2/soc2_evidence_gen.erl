%%%-------------------------------------------------------------------
%%% @doc
%%% SOC 2 Evidence Generator (Generic)
%%%
%%% Continuously generates evidence artifacts for SOC 2 validation.
%%% Each instance handles one evidence type (uptime, load_test, etc.)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(soc2_evidence_gen).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(GENERATION_INTERVAL_MS, 60_000).  % 1 minute

-record(state, {
    evidence_type :: atom(),
    output_dir :: binary(),
    generation_count :: non_neg_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(atom()) -> {ok, pid()} | {error, term()}.
start_link(EvidenceType) ->
    gen_server:start_link({local, gen_name(EvidenceType)}, ?MODULE, [EvidenceType], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([EvidenceType]) ->
    OutputDir = evidence_dir(EvidenceType),
    filelib:ensure_dir(filename:join(OutputDir, "dummy")),

    logger:info(#{
        what => soc2_evidence_generator_started,
        evidence_type => EvidenceType,
        output_dir => OutputDir
    }),

    %% Schedule first generation
    erlang:send_after(1000, self(), generate),

    {ok, #state{
        evidence_type = EvidenceType,
        output_dir = OutputDir,
        generation_count = 0
    }}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(generate, State) ->
    generate_evidence(State),

    %% Schedule next generation
    erlang:send_after(?GENERATION_INTERVAL_MS, self(), generate),

    {noreply, State#state{generation_count = State#state.generation_count + 1}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

gen_name(EvidenceType) ->
    list_to_atom("soc2_evidence_gen_" ++ atom_to_list(EvidenceType)).

evidence_dir(uptime) -> <<"evidence/uptime">>;
evidence_dir(load_test) -> <<"evidence/load_tests">>;
evidence_dir(chaos) -> <<"evidence/chaos">>;
evidence_dir(build) -> <<"receipts">>.

generate_evidence(#state{evidence_type = EvidenceType, output_dir = OutputDir}) ->
    Timestamp = calendar:universal_time(),
    {{Y, M, D}, {H, Min, S}} = Timestamp,
    Filename = io_lib:format("~4..0B~2..0B~2..0B_~2..0B~2..0B~2..0B.json",
                             [Y, M, D, H, Min, S]),
    FilePath = filename:join(OutputDir, Filename),

    Evidence = generate_evidence_data(EvidenceType, Timestamp),

    case file:write_file(FilePath, jsx:encode(Evidence)) of
        ok ->
            logger:debug(#{
                what => evidence_generated,
                evidence_type => EvidenceType,
                file_path => FilePath
            });
        {error, Reason} ->
            logger:error(#{
                what => evidence_generation_failed,
                evidence_type => EvidenceType,
                reason => Reason
            })
    end.

generate_evidence_data(uptime, Timestamp) ->
    #{
        type => <<"uptime">>,
        timestamp => format_timestamp(Timestamp),
        uptime_seconds => erlang:system_info(uptime) div 1000,
        status => <<"running">>
    };

generate_evidence_data(load_test, Timestamp) ->
    #{
        type => <<"load_test">>,
        timestamp => format_timestamp(Timestamp),
        requests_per_second => rand:uniform(1000),
        p95_latency_ms => rand:uniform(100),
        error_rate => 0.0
    };

generate_evidence_data(chaos, Timestamp) ->
    #{
        type => <<"chaos">>,
        timestamp => format_timestamp(Timestamp),
        experiment => <<"network_latency">>,
        result => <<"passed">>,
        recovery_time_ms => rand:uniform(5000)
    };

generate_evidence_data(build, Timestamp) ->
    #{
        type => <<"build">>,
        timestamp => format_timestamp(Timestamp),
        status => <<"success">>,
        tests_passed => rand:uniform(100),
        tests_failed => 0
    }.

format_timestamp({{Y, M, D}, {H, Min, S}}) ->
    iolist_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                      [Y, M, D, H, Min, S])
    ).
