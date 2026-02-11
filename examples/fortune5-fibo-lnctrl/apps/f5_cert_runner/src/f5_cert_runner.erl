%% Certification Pipeline Orchestrator
%% Coordinates all evidence collection activities
-module(f5_cert_runner).
-behaviour(gen_server).

-export([start_link/0, start_collection/0, generate_report/0, get_status/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    evidence_modules = [f5_uptime_logger, f5_load_tester, chaos_controller] :: [atom()],
    collection_count = 0 :: integer(),
    last_collection_time :: integer() | undefined,
    errors = [] :: [term()]
}).

%%% API

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec start_collection() -> ok | {error, term()}.
start_collection() ->
    gen_server:call(?MODULE, start_collection).

-spec generate_report() -> {ok, map()} | {error, term()}.
generate_report() ->
    gen_server:call(?MODULE, generate_report, 60000).

-spec get_status() -> map().
get_status() ->
    gen_server:call(?MODULE, get_status).

%%% gen_server callbacks

-spec init([]) -> {ok, #state{}}.
init([]) ->
    logger:info("Certification runner started"),

    %% Start all evidence collection modules
    State = #state{},
    lists:foreach(fun(Module) ->
        case Module:start() of
            ok ->
                logger:info("Started evidence collector: ~p", [Module]);
            {error, Reason} ->
                logger:error("Failed to start evidence collector ~p: ~p", [Module, Reason])
        end
    end, State#state.evidence_modules),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(start_collection, _From, State = #state{evidence_modules = Modules, collection_count = Count}) ->
    logger:info("Starting evidence collection cycle ~p", [Count + 1]),

    Results = lists:map(fun(Module) ->
        try
            case Module:collect() of
                {ok, Evidence} ->
                    logger:info("Collected evidence from ~p", [Module]),
                    {Module, ok, Evidence};
                {error, Reason} ->
                    logger:error("Failed to collect evidence from ~p: ~p", [Module, Reason]),
                    {Module, error, Reason}
            end
        catch
            Class:Reason:Stacktrace ->
                logger:error("Exception collecting evidence from ~p: ~p:~p~n~p",
                           [Module, Class, Reason, Stacktrace]),
                {Module, error, {exception, Class, Reason}}
        end
    end, Modules),

    NewState = State#state{
        collection_count = Count + 1,
        last_collection_time = erlang:system_time(second),
        errors = [R || {_, error, R} <- Results]
    },

    {reply, {ok, Results}, NewState};

handle_call(generate_report, _From, State) ->
    logger:info("Generating certification report"),

    try
        Report = generate_certification_report:generate(),
        {reply, {ok, Report}, State}
    catch
        Class:Reason:Stacktrace ->
            logger:error("Failed to generate report: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            {reply, {error, {exception, Class, Reason}}, State}
    end;

handle_call(get_status, _From, State = #state{collection_count = Count, last_collection_time = LastTime, errors = Errors}) ->
    Status = #{
        collection_count => Count,
        last_collection_time => LastTime,
        errors => Errors,
        uptime_seconds => erlang:system_time(second) - element(1, erlang:statistics(wall_clock)) div 1000
    },
    {reply, Status, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, #state{evidence_modules = Modules}) ->
    %% Stop all evidence collectors
    lists:foreach(fun(Module) ->
        try
            Module:stop()
        catch
            _:_ -> ok
        end
    end, Modules),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
