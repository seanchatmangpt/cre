%%%-------------------------------------------------------------------
%%% @doc evidence_hooks - Trace pattern hooks for automatic effect counting.
%%%
%%% This module installs Erlang trace patterns that automatically count
%%% workflow effects by monitoring function calls in workflow modules.
%%%
%%% <h3>Traced Modules</h3>
%%% <ul>
%%%   <li><b>ln_cancel:</b> Cancellation operations</li>
%%%   <li><b>ln_join:</b> Join operations</li>
%%%   <li><b>ln_loop:</b> Loop/fork operations</li>
%%%   <li><b>ln_effect:</b> Effect operations</li>
%%%   <li><b>wf_audit_log:</b> Receipt generation</li>
%%%   <li><b>gen_pnet:</b> Task lifecycle (if available)</li>
%%% </ul>
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(evidence_hooks).

%% API
-export([install_trace_hooks/0]).
-export([uninstall_trace_hooks/0]).
-export([install_scope_hooks/0]).
-export([is_installed/0]).

%%====================================================================
%% Types
%%====================================================================

-type effect_type() ::
    task_start |
    task_complete |
    cancel |
    fork |
    join |
    scope_enter |
    scope_exit |
    wait_signal |
    effect_receipt.

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Installs trace patterns for automatic effect counting.
%%
%% Sets up trace patterns on key workflow modules that send
%% messages to evidence_counter for each effect type.
%%
%% @returns {ok, InstalledCount} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec install_trace_hooks() -> {ok, non_neg_integer()} | {error, term()}.
install_trace_hooks() ->
    case whereis(evidence_counter) of
        undefined ->
            {error, evidence_counter_not_running};
        _Pid ->
            uninstall_trace_hooks(),
            Patterns = get_hook_patterns(),
            install_patterns(Patterns, 0)
    end.

%%--------------------------------------------------------------------
%% @doc Uninstalls all trace patterns.
%%
%% Clears any active trace patterns set by this module.
%%
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec uninstall_trace_hooks() -> ok.
uninstall_trace_hooks() ->
    %% Clear all trace patterns
    erlang:trace_pattern({'_', '_', '_'}, false, [local]),
    ok.

%%--------------------------------------------------------------------
%% @doc Installs scope entry/exit hooks.
%%
%% Additional hooks for tracking scope boundaries specifically.
%%
%% @returns {ok, InstalledCount} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec install_scope_hooks() -> {ok, non_neg_integer()} | {error, term()}.
install_scope_hooks() ->
    case whereis(evidence_counter) of
        undefined ->
            {error, evidence_counter_not_running};
        _Pid ->
            %% Scope hooks would be installed on specific scope functions
            %% For now, this is a placeholder for future scope tracking
            {ok, 0}
    end.

%%--------------------------------------------------------------------
%% @doc Checks if trace hooks are currently installed.
%%
%% @returns true if hooks are installed, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec is_installed() -> boolean().
is_installed() ->
    %% Check if any trace patterns are active
    case erlang:trace_info({evidence_counter, count_effect, 2}, enabled) of
        {enabled, _} -> true;
        _ -> false
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Gets the list of hook patterns to install.
%% Returns list of {Module, Function, Arity, EffectType, MatchSpec}
-spec get_hook_patterns() -> [{module(), atom(), arity(), effect_type(), ets:match_spec()}].
get_hook_patterns() ->
    [
        %% ln_cancel hooks
        {ln_cancel, request, 3, cancel, build_match_spec(cancel)},
        {ln_cancel, execute, 1, cancel, build_match_spec(cancel)},

        %% ln_join hooks
        {ln_join, wait, 2, join, build_match_spec(join)},
        {ln_join, sync, 1, join, build_match_spec(join)},

        %% ln_effect hooks
        {ln_effect, request, 4, effect_receipt, build_match_spec(effect_receipt)},
        {ln_effect, complete, 3, effect_receipt, build_match_spec(effect_receipt)},

        %% wf_audit_log hooks
        {wf_audit_log, append, 2, effect_receipt, build_match_spec(effect_receipt)}
    ].

%% @private
%% @doc Builds a match spec for tracing function calls.
%% The match spec sends a message to the counter process.
%% Uses a tuple format {count_effect, EffectType} which gets converted internally.
-spec build_match_spec(effect_type()) -> ets:match_spec().
build_match_spec(EffectType) ->
    %% Simple message: just the effect type, details will be empty
    %% The match spec action format: {message, Dest}
    %% where Dest is {evidence_counter, count_effect, [EffectType, #{}]}
    %% but we need to construct the map differently
    [{'_', [], [{message, {evidence_counter, count_effect_tuple, [EffectType]}}]}].

%% @private
%% @doc Installs trace patterns from the list.
-spec install_patterns([{module(), atom(), arity(), effect_type(), ets:match_spec()}], non_neg_integer()) ->
    {ok, non_neg_integer()}.
install_patterns([], Count) ->
    {ok, Count};
install_patterns([{Module, Function, Arity, _EffectType, MatchSpec} | Rest], Count) ->
    case is_module_loaded(Module) of
        true ->
            %% Install trace pattern for this function
            try
                erlang:trace_pattern({Module, Function, Arity}, MatchSpec, [local]),
                install_patterns(Rest, Count + 1)
            catch
                _:_ ->
                    install_patterns(Rest, Count)
            end;
        false ->
            %% Module not loaded, skip this pattern
            install_patterns(Rest, Count)
    end.

%% @private
%% @doc Checks if a module is loaded.
-spec is_module_loaded(module()) -> boolean().
is_module_loaded(Module) ->
    case code:is_loaded(Module) of
        {file, _} -> true;
        false -> false
    end.
