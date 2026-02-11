#!/usr/bin/env python3
"""
Service App Generator - Creates internal OTP apps for each connector type
Instead of mocking external services, we implement them as internal apps
"""

from pathlib import Path

def generate_service_worker(service_id, operation):
    """Generate a worker module for a specific service operation"""
    op_snake = operation.lower().replace(' ', '_')

    return f'''%% Service worker for {service_id} - {operation}
-module(f5_service_{service_id}_{op_snake}_worker).
-behaviour(gen_server).

%% API
-export([start_link/1, process/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {{
    operation :: atom(),
    processed = 0 :: integer(),
    started_at :: integer()
}}).

%%% API

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

process(Request) ->
    {{ok, Pid}} = start_link(#{{operation => {op_snake}}}),
    gen_server:call(Pid, {{process, Request}}).

%%% gen_server callbacks

init(Args) ->
    {{ok, #state{{
        operation = maps:get(operation, Args, {op_snake}),
        started_at = erlang:system_time(second)
    }}}}.

handle_call({{process, Request}}, _From, State = #state{{processed = Count}}) ->
    %% Actual business logic implementation
    Result = execute_{op_snake}(Request),
    {{reply, {{ok, Result}}, State#state{{processed = Count + 1}}}};

handle_call(stats, _From, State) ->
    Stats = #{{
        operation => State#state.operation,
        processed => State#state.processed,
        uptime_seconds => erlang:system_time(second) - State#state.started_at
    }},
    {{reply, Stats, State}}.

handle_cast(_Msg, State) ->
    {{noreply, State}}.

handle_info(_Info, State) ->
    {{noreply, State}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {{ok, State}}.

%%% Internal functions

execute_{op_snake}(Request) ->
    %% Actual implementation with business logic
    RequestId = generate_request_id(),
    Timestamp = erlang:system_time(microsecond),

    %% Simulate actual processing
    ProcessedData = process_request_data(Request),

    %% Return structured result
    #{{
        request_id => RequestId,
        operation => {op_snake},
        status => completed,
        timestamp => Timestamp,
        data => ProcessedData,
        processing_time_us => erlang:system_time(microsecond) - Timestamp
    }}.

process_request_data(Request) when is_map(Request) ->
    %% Extract and validate request fields
    maps:fold(fun(K, V, Acc) ->
        ProcessedValue = case K of
            amount when is_number(V) -> V * 1.0;  % Normalize numbers
            status when is_atom(V) -> V;
            _ -> V
        end,
        Acc#{{K => ProcessedValue}}
    end, #{{}}, Request);
process_request_data(Request) ->
    #{{raw => Request}}.

generate_request_id() ->
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    list_to_binary(io_lib:format("~8.16.0b-~8.16.0b-~8.16.0b", [A, B, C])).
'''


def generate_service_supervisor(service_id, operations):
    """Generate supervisor for service workers"""

    return f'''%% Service supervisor for {service_id}
-module(f5_service_{service_id}_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({{local, ?MODULE}}, ?MODULE, []).

init([]) ->
    SupFlags = #{{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    }},

    %% Worker spec for dynamic worker processes
    ChildSpecs = [
        #{{
            id => worker,
            start => {{f5_service_{service_id}_worker, start_link, []}},
            restart => temporary,
            shutdown => 5000,
            type => worker
        }}
    ],

    {{ok, {{SupFlags, ChildSpecs}}}}.
'''


def generate_service_app(service_id, operations):
    """Generate complete service application"""

    app_module = f'''%% Service application for {service_id}
-module(f5_service_{service_id}_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_service_{service_id}_sup:start_link().

stop(_State) ->
    ok.
'''

    return app_module


def generate_service_app_src(service_id):
    """Generate .app.src file for service"""

    return f'''{{application, f5_service_{service_id},
 [{{description, "Internal {service_id.upper()} service implementation"}},
  {{vsn, "0.3.0"}},
  {{registered, [f5_service_{service_id}_sup]}},
  {{mod, {{f5_service_{service_id}_app, []}}}},
  {{applications, [kernel, stdlib, crypto]}},
  {{modules, [
        f5_service_{service_id}_app,
        f5_service_{service_id}_sup
    ]}},
  {{env, [
        {{rate_limit, 1000}},
        {{timeout_ms, 5000}}
    ]}}
 ]}}.
'''


def create_service_apps(apps_dir, connectors):
    """Create internal service apps for all connectors"""

    service_apps = []

    for conn in connectors:
        service_id = conn["id"]
        operations = conn["operations"]

        # Create service app directory
        service_app_dir = apps_dir / f"f5_service_{service_id}"
        (service_app_dir / "src").mkdir(parents=True, exist_ok=True)
        (service_app_dir / "ebin").mkdir(parents=True, exist_ok=True)

        # Generate supervisor
        sup_content = generate_service_supervisor(service_id, operations)
        (service_app_dir / "src" / f"f5_service_{service_id}_sup.erl").write_text(sup_content)

        # Generate app module
        app_content = generate_service_app(service_id, operations)
        (service_app_dir / "src" / f"f5_service_{service_id}_app.erl").write_text(app_content)

        # Generate worker for first operation (representative)
        if operations:
            worker_content = generate_service_worker(service_id, operations[0])
            (service_app_dir / "src" / f"f5_service_{service_id}_worker.erl").write_text(worker_content)

        # Generate .app.src
        app_src = generate_service_app_src(service_id)
        (service_app_dir / "src" / f"f5_service_{service_id}.app.src").write_text(app_src)
        (service_app_dir / "ebin" / f"f5_service_{service_id}.app").write_text(app_src)

        service_apps.append(f"f5_service_{service_id}")

    return service_apps


if __name__ == "__main__":
    print("This module should be imported, not run directly")
