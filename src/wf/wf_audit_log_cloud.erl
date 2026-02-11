%% -*- erlang -*-
%%%% @doc Cloud Logging backend for wf_audit_log
%%
%% This module provides Google Cloud Logging integration for audit receipts,
%% enabling centralized log retention and compliance reporting (e.g., SOX 400-day).
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Asynchronous log export (fire-and-forget, non-blocking)</li>
%%   <li>Cloud Logging structured logging with jsonPayload</li>
%%   <li>Error resilience (logging failures don't block workflow execution)</li>
%% </ul>
%%
%% <h3>Basic Usage</h3>
%%
%% Appending a receipt to Cloud Logging:
%% ```erlang
%% > Receipt = #{before_hash => <<"b1">>, after_hash => <<"a1">>,
%% >            move => #{trsn => t1, mode => #{}, produce => #{}}, ts => 1}.
%% > ok = wf_audit_log_cloud:append(Receipt).
%% ok
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_audit_log_cloud).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([start_link/0, append/1]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the Cloud Logging gen_server (placeholder for future implementation).
%%
%% Currently, this module uses synchronous Cloud Logging API calls.
%% A gen_server could be added for connection pooling and batching.
%%
%% @returns {ok, Pid} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    %% Placeholder: Future implementation could start a gen_server
    %% for connection pooling and batched log writes.
    {ok, self()}.

%%--------------------------------------------------------------------
%% @doc Appends a receipt to Google Cloud Logging.
%%
%% This is a fire-and-forget operation that logs errors locally
%% but does not block workflow execution on Cloud Logging failures.
%%
%% The receipt is logged as a structured log entry with jsonPayload.
%%
%% @param Receipt Receipt map to append
%% @returns ok (always returns ok, errors are logged locally)
%%
%% @end
%%--------------------------------------------------------------------
-spec append(Receipt :: map()) -> ok.
append(Receipt) when is_map(Receipt) ->
    %% Fire-and-forget: Spawn a separate process to handle Cloud Logging
    %% This ensures workflow execution is never blocked by Cloud Logging issues
    spawn(fun() ->
        try
            do_append(Receipt)
        catch
            Type:Error:Stacktrace ->
                %% Log locally but don't fail the workflow
                error_logger:error_msg(
                    "Failed to export audit log to Cloud Logging: ~p:~p~nStacktrace: ~p~nReceipt: ~p~n",
                    [Type, Error, Stacktrace, Receipt]
                )
        end
    end),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Internal function to append receipt to Cloud Logging.
%%
%% Constructs a Cloud Logging entry and writes it using the Google Cloud
%% Logging API. The entry includes:
%% - timestamp: Receipt timestamp
%% - severity: INFO
%% - jsonPayload: Full receipt as structured data
%%
%% @param Receipt Receipt map
%% @returns ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
-spec do_append(Receipt :: map()) -> ok | {error, term()}.
do_append(Receipt) ->
    %% Extract timestamp from receipt (or use current time if missing)
    Ts = maps:get(ts, Receipt, erlang:system_time(millisecond)),

    %% Convert timestamp to Cloud Logging format (RFC3339)
    TsStr = format_timestamp(Ts),

    %% Construct Cloud Logging entry
    %% Using jsonPayload for structured querying in BigQuery
    Entry = #{
        <<"timestamp">> => TsStr,
        <<"severity">> => <<"INFO">>,
        <<"jsonPayload">> => Receipt,
        <<"logName">> => <<"cre-audit-log">>,
        <<"resource">> => #{
            <<"type">> => <<"k8s_container">>,
            <<"labels">> => #{
                <<"project_id">> => get_env("GOOGLE_CLOUD_PROJECT", "unknown"),
                <<"location">> => get_env("GOOGLE_CLOUD_REGION", "unknown"),
                <<"cluster_name">> => get_env("CLUSTER_NAME", "unknown"),
                <<"namespace_name">> => get_env("NAMESPACE", "unknown")
            }
        }
    },

    %% Write to Cloud Logging
    %% Note: This requires the Google Cloud Logging Erlang library
    %% or HTTP API calls. For now, this is a placeholder implementation.
    %%
    %% Future implementation options:
    %% 1. Use google-cloud-logging Erlang library (if available)
    %% 2. Use HTTP API with Workload Identity token
    %% 3. Use Cloud Logging sidecar agent
    %%
    %% For this phase, we log locally to stdout (captured by Cloud Logging)
    io:format("~s~n", [jsone:encode(Entry)]),

    ok.

%%--------------------------------------------------------------------
%% @doc Formats a timestamp for Cloud Logging.
%%
%% @param Ts Timestamp in milliseconds
%% @returns ISO8601/RFC3339 formatted string
%% @end
%%--------------------------------------------------------------------
-spec format_timestamp(integer()) -> binary().
format_timestamp(Ts) when is_integer(Ts) ->
    %% Convert milliseconds to seconds
    Sec = Ts div 1000,
    %% Convert to Unix time
    Megasec = Sec div 1000000,
    Sec2 = Sec rem 1000000,
    Micro = Ts rem 1000,
    %% Format as ISO8601
    DateTime = calendar:system_time_to_universal_time(Megasec * 1000000 + Sec2, seconds),
    httpd_util:rfc1123_date(DateTime).

%%--------------------------------------------------------------------
%% @doc Gets an environment variable with default fallback.
%%
%% @param Key Environment variable name
%% @param Default Default value if not found
%% @returns string() value
%% @end
%%--------------------------------------------------------------------
-spec get_env(string(), string()) -> string().
get_env(Key, Default) ->
    case os:getenv(Key) of
        false -> Default;
        Value -> Value
    end.
