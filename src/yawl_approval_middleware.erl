%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Approval Middleware for YAWL Workflow Patterns
%%
%% This module provides middleware functionality for wrapping YAWL workflow
%% patterns with approval checkpoints, enabling human-in-the-loop execution.
%%
%% @author CRE Team
%% @version 1.0.0
%% @doc YAWL Approval Middleware
%%
%% <h3>Overview</h3>
%% This module provides middleware patterns for approval-wrapped workflows:
%% <ul>
%%   <li>Wrap any pattern with approval middleware</li>
%%   <li>Approve before critical steps</li>
%%   <li>Conditional approval based on context</li>
%%   <li>Composable middleware chains</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_approval_middleware).
-author("CRE Team").

%% Include record definitions
-include("cre_yawl.hrl").
-include("cre_yawl_patterns.hrl").

%%====================================================================
%% Exports
%%====================================================================

%% Main API
-export([
    with_approval/2,
    with_approval/3,
    approve_before/2,
    approve_after_execution/2,
    approve_around/3,
    conditional_approval/3,
    chain_approval/2
]).

%% Middleware helpers
-export([
    wrap_executor/2,
    unwrap/1,
    is_wrapped/1
]).

%%====================================================================
%% Types
%%====================================================================

-type approval_config() :: #{
    required_approver := human | simulated | auto,
    timeout => non_neg_integer() | infinity,
    approval_schema => map(),
    context => map(),
    on_denied => continue | stop | rollback,
    on_timeout => continue | stop | rollback
}.

-type middleware_fun() :: fun((term()) -> term()).
-type approval_result() :: {ok, term()} | {error, term()}.

-export_type([approval_config/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Wraps a pattern with approval middleware.
%%
%% @param Pattern The pattern to wrap.
%% @param Config The approval configuration.
%% @return Wrapped pattern record.
%%
%% @end
%%--------------------------------------------------------------------
-spec with_approval(term(), approval_config()) -> #approval_wrapped{}.

with_approval(Pattern, Config) ->
    with_approval(Pattern, Config, 'before').

%%--------------------------------------------------------------------
%% @doc Wraps a pattern with approval at a specific point.
%%
%% @param Pattern The pattern to wrap.
%% @param Config The approval configuration.
%% @param Position 'before' | 'after_execution' | 'around'.
%% @return Wrapped pattern record.
%%
%% @end
%%--------------------------------------------------------------------
-spec with_approval(term(), approval_config(), atom()) -> #approval_wrapped{}.

with_approval(Pattern, Config, Position) ->
    Wrapped = #approval_wrapped{
        original_pattern = Pattern,
        approval_config = Config,
        middleware_chain = []
    },
    case Position of
        'before' -> approve_before(Wrapped, Config);
        'after_execution' -> approve_after_execution(Wrapped, Config);
        'around' -> approve_around(Wrapped, Config, fun execute_original/1)
    end.

%%--------------------------------------------------------------------
%% @doc Creates middleware that approves before execution.
%%
%% @param Wrapped The wrapped pattern.
%% @param Config The approval configuration.
%% @return Updated wrapped pattern with before middleware.
%%
%% @end
%%--------------------------------------------------------------------
-spec approve_before(#approval_wrapped{}, approval_config()) -> #approval_wrapped{}.

approve_before(Wrapped, Config) ->
    MiddlewareFun = fun(Pattern) ->
        %% Create checkpoint before execution
        StepName = get_step_name(Pattern),
        Context = maps:get(context, Config, #{}),
        Options = maps:with([required_approver, timeout, approval_schema], Config),
        FullContext = maps:merge(Context, Options),

        case yawl_approval:create_checkpoint(StepName, StepName,
                maps:put(context, FullContext, Options)) of
            {ok, CheckpointId} ->
                case yawl_approval:request_approval(CheckpointId) of
                    {ok, #approval_decision{approved = true}} ->
                        %% Approved, execute pattern
                        execute_pattern(Pattern);
                    {ok, #approval_decision{approved = false, reason = Reason}} ->
                        %% Denied
                        handle_denied(Pattern, Reason, Config);
                    {error, timeout} ->
                        handle_timeout(Pattern, Config)
                end;
            {error, Reason} ->
                {error, {checkpoint_creation_failed, Reason}}
        end
    end,

    Wrapped#approval_wrapped{
        middleware_chain = [MiddlewareFun | Wrapped#approval_wrapped.middleware_chain]
    }.

%%--------------------------------------------------------------------
%% @doc Creates middleware that approves after execution.
%%
%% @param Wrapped The wrapped pattern.
%% @param Config The approval configuration.
%% @return Updated wrapped pattern with after middleware.
%%
%% @end
%%--------------------------------------------------------------------
-spec approve_after_execution(#approval_wrapped{}, approval_config()) -> #approval_wrapped{}.

approve_after_execution(Wrapped, Config) ->
    MiddlewareFun = fun(Pattern) ->
        %% Execute pattern first
        Result = execute_pattern(Pattern),

        %% Then request approval for the result
        case Result of
            {ok, ExecResult} ->
                StepName = get_step_name(Pattern),
                BaseContext = maps:get(context, Config, #{}),
                Context = maps:put(result, ExecResult, BaseContext),
                Options = maps:with([required_approver, timeout, approval_schema], Config),
                FullContext = maps:merge(Context, Options),

                case yawl_approval:create_checkpoint(StepName, StepName,
                        maps:put(context, FullContext, Options)) of
                    {ok, CheckpointId} ->
                        case yawl_approval:request_approval(CheckpointId) of
                            {ok, #approval_decision{approved = true}} ->
                                %% Approved, return result
                                Result;
                            {ok, #approval_decision{approved = false, reason = Reason}} ->
                                %% Denied after execution
                                handle_denied(Pattern, Reason, Config);
                            {error, timeout} ->
                                handle_timeout(Pattern, Config)
                        end;
                    {error, _Reason} ->
                        %% On checkpoint failure, still return result
                        Result
                end;
            {error, _} = Error ->
                Error
        end
    end,

    Wrapped#approval_wrapped{
        middleware_chain = [MiddlewareFun | Wrapped#approval_wrapped.middleware_chain]
    }.

%%--------------------------------------------------------------------
%% @doc Creates middleware that approves around execution.
%%
%% @param Wrapped The wrapped pattern.
%% @param Config The approval configuration.
%% @param ExecuteFun The function to execute the pattern.
%% @return Updated wrapped pattern with around middleware.
%%
%% @end
%%--------------------------------------------------------------------
-spec approve_around(#approval_wrapped{}, approval_config(), middleware_fun()) ->
    #approval_wrapped{}.

approve_around(Wrapped, Config, ExecuteFun) ->
    MiddlewareFun = fun(Pattern) ->
        %% Pre-approval
        StepName = get_step_name(Pattern),
        BaseContext = maps:get(context, Config, #{}),
        PreContext = maps:put(phase, pre_execution, BaseContext),
        Options = maps:with([required_approver, timeout, approval_schema], Config),
        FullPreContext = maps:merge(PreContext, Options),

        case yawl_approval:create_checkpoint(StepName, StepName,
                maps:put(context, FullPreContext, Options)) of
            {ok, PreCheckpointId} ->
                case yawl_approval:request_approval(PreCheckpointId) of
                    {ok, #approval_decision{approved = true}} ->
                        %% Execute
                        Result = ExecuteFun(Pattern),

                        %% Post-approval
                        case Result of
                            {ok, ExecResult} ->
                                PostContext = maps:put(result, ExecResult,
                                    maps:put(phase, post_execution, BaseContext)),
                                FullPostContext = maps:merge(PostContext, Options),

                                case yawl_approval:create_checkpoint(StepName, StepName,
                                        maps:put(context, FullPostContext, Options)) of
                                    {ok, PostCheckpointId} ->
                                        case yawl_approval:request_approval(PostCheckpointId) of
                                            {ok, #approval_decision{approved = true}} ->
                                                Result;
                                            {ok, #approval_decision{approved = false, reason = Reason}} ->
                                                handle_denied(Pattern, Reason, Config);
                                            {error, timeout} ->
                                                handle_timeout(Pattern, Config)
                                        end;
                                    {error, _} ->
                                        Result
                                end;
                            {error, _} = Error ->
                                Error
                        end;
                    {ok, #approval_decision{approved = false, reason = Reason}} ->
                        handle_denied(Pattern, Reason, Config);
                    {error, timeout} ->
                        handle_timeout(Pattern, Config)
                end;
            {error, Reason} ->
                {error, {checkpoint_creation_failed, Reason}}
        end
    end,

    Wrapped#approval_wrapped{
        middleware_chain = [MiddlewareFun | Wrapped#approval_wrapped.middleware_chain]
    }.

%%--------------------------------------------------------------------
%% @doc Creates conditional approval middleware.
%%
%% @param Pattern The pattern to wrap.
%% @param Config The approval configuration.
%% @param ConditionFun Function that returns true if approval needed.
%% @return Wrapped pattern or original pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec conditional_approval(term(), approval_config(), fun(() -> boolean())) ->
    term().

conditional_approval(Pattern, Config, ConditionFun) ->
    case ConditionFun() of
        true -> with_approval(Pattern, Config);
        false -> Pattern
    end.

%%--------------------------------------------------------------------
%% @doc Chains multiple approval middleware together.
%%
%% @param Pattern The pattern to wrap.
%% @param Configs List of approval configurations.
%% @return Wrapped pattern with chained middleware.
%%
%% @end
%%--------------------------------------------------------------------
-spec chain_approval(term(), [approval_config()]) -> #approval_wrapped{}.

chain_approval(Pattern, Configs) when is_list(Configs) ->
    lists:foldl(fun(Config, AccPattern) ->
        with_approval(AccPattern, Config)
    end, Pattern, Configs).

%%--------------------------------------------------------------------
%% @doc Wraps the executor to handle approval-wrapped patterns.
%%
%% @param Pattern The pattern to execute.
%% @param Input The input data.
%% @return Execution result.
%%
%% @end
%%--------------------------------------------------------------------
-spec wrap_executor(term(), term()) -> approval_result().

wrap_executor(Pattern, Input) ->
    case is_wrapped(Pattern) of
        true ->
            %% Execute middleware chain
            execute_chain(Pattern, Input);
        false ->
            %% Not wrapped, execute normally
            yawl_executor:execute_pattern(Pattern, Input)
    end.

%%--------------------------------------------------------------------
%% @doc Unwraps an approval-wrapped pattern.
%%
%% @param Wrapped The wrapped pattern.
%% @return The original pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec unwrap(#approval_wrapped{} | term()) -> term().

unwrap(#approval_wrapped{original_pattern = Pattern}) -> Pattern;
unwrap(Pattern) -> Pattern.

%%--------------------------------------------------------------------
%% @doc Checks if a pattern is approval-wrapped.
%%
%% @param Pattern The pattern to check.
%% @return true if wrapped, false otherwise.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_wrapped(term()) -> boolean().

is_wrapped(#approval_wrapped{}) -> true;
is_wrapped(_) -> false.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Executes the original pattern.
%%
%% @end
%%--------------------------------------------------------------------
execute_original(Pattern) ->
    yawl_executor:execute_pattern(Pattern, #{}).

%%--------------------------------------------------------------------
%% @private
%% @doc Executes a pattern directly.
%%
%% @end
%%--------------------------------------------------------------------
execute_pattern(Pattern) ->
    try
        case Pattern of
            #approval_wrapped{} -> execute_chain(Pattern, #{});
            _ -> yawl_executor:execute_pattern(Pattern, #{})
        end
    catch
        Kind:Reason:Stack ->
            {error, {execution_error, Kind, Reason, Stack}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Executes the middleware chain.
%%
%% @end
%%--------------------------------------------------------------------
execute_chain(#approval_wrapped{
    original_pattern = OriginalPattern,
    middleware_chain = Chain
}, Input) ->
    %% Apply middleware in order
    InitialFun = fun(P) -> yawl_executor:execute_pattern(P, Input) end,

    ExecFun = lists:foldl(fun(Middleware, AccFun) ->
        fun(P) ->
            case Middleware(P) of
                {ok, _} = Result -> Result;
                {error, _} = Error -> Error;
                Other -> {ok, Other}
            end
        end
    end, InitialFun, lists:reverse(Chain)),

    ExecFun(OriginalPattern).

%%--------------------------------------------------------------------
%% @private
%% @doc Handles denied approval.
%%
%% @end
%%--------------------------------------------------------------------
handle_denied(_Pattern, Reason, Config) ->
    OnDenied = maps:get(on_denied, Config, stop),
    case OnDenied of
        continue ->
            logger:info("Approval denied but continuing: ~p", [Reason]),
            {ok, #{denied => true, reason => Reason}};
        stop ->
            {error, {approval_denied, Reason}};
        rollback ->
            logger:warning("Approval denied, rolling back: ~p", [Reason]),
            {error, {approval_denied_rollback, Reason}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles approval timeout.
%%
%% @end
%%--------------------------------------------------------------------
handle_timeout(_Pattern, Config) ->
    OnTimeout = maps:get(on_timeout, Config, stop),
    case OnTimeout of
        continue ->
            logger:info("Approval timeout but continuing"),
            {ok, #{timeout => true}};
        stop ->
            {error, approval_timeout};
        rollback ->
            logger:warning("Approval timeout, rolling back"),
            {error, {approval_timeout_rollback}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Gets the step name from a pattern.
%%
%% @end
%%--------------------------------------------------------------------
get_step_name(Pattern) ->
    case Pattern of
        #approval_wrapped{original_pattern = Original} ->
            get_step_name(Original);
        #wcp_pattern{name = Name} ->
            binary_to_atom(Name, utf8);
        #pattern_state{pattern_type = Type} ->
            Type;
        #sequence{} -> sequence;
        #parallel_split{} -> parallel_split;
        #synchronization{} -> synchronization;
        #exclusive_choice{} -> exclusive_choice;
        _ ->
            case element(1, Pattern) of
                RecordName when is_atom(RecordName) -> RecordName;
                _ -> unknown_step
            end
    end.
