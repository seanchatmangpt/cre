%% -*- erlang -*-
%%%% @doc ga_constitution - Header file for constitution records.
%%

-ifndef(GA_CONSTITUTION_HRL).
-define(GA_CONSTITUTION_HRL, true).

%%--------------------------------------------------------------------
%% @doc Constitution record for GA compiler.
%%
%% Contains all components needed to compile a workflow specification
%% into executable gen_yawl modules.
%%--------------------------------------------------------------------
-record(constitution, {
    id :: binary(),
    version :: binary(),
    sigma = #{} :: map(),
    refusals = [] :: [ga_constitution:refusal()],
    quality_gates = [] :: [ga_constitution:quality_gate()],
    lambda :: ga_constitution:lambda()
}).

-record(sigma, {
    type_system = behavioral :: behavioral | static | dynamic,
    type_bindings = [] :: [ga_constitution:type_binding()]
}).

-record(type_binding, {
    term :: binary(),
    type :: binary(),
    token_contract :: #{
        shape := singleton | multiple | optional,
        validity := eager | lazy
    }
}).

-record(refusal, {
    state :: binary(),
    refused_transitions = [] :: [binary()],
    refusal_reason :: binary()
}).

-record(quality_gate, {
    name :: binary(),
    invariant :: binary(),
    replay_enabled = false :: boolean(),
    provenance_enabled = false :: boolean(),
    receipt_required = false :: boolean()
}).

-record(lambda, {
    compilation_strategy = sequential :: sequential | parallel | topological,
    pattern_sequence = [] :: [ga_constitution:pattern_instance()]
}).

-record(pattern_instance, {
    pattern :: binary(),
    instance_id :: binary(),
    config = #{} :: map()
}).

-record(token_contract, {
    shape :: singleton | multiple | optional,
    validity :: eager | lazy,
    lifespan :: temporary | permanent
}).

%% Type definitions
-type constitution() :: #constitution{}.
-type sigma() :: #sigma{}.
-type type_binding() :: #type_binding{}.
-type refusal() :: #refusal{}.
-type quality_gate() :: #quality_gate{}.
-type lambda() :: #lambda{}.
-type pattern_instance() :: #pattern_instance{}.
-type token_contract() :: #token_contract{}.

-endif.
