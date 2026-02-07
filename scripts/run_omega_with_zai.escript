#!/usr/bin/env escript
%% -*- erlang -*-
%% @doc Run AGI Symposium Ω simulation with Z.AI LLM-backed human task decisions.
%%
%% Requires ZAI_API_KEY or cre.zai_api_key. Loads omega YAML, compiles,
%% loads modules, starts gen_yawl, and runs participant loop with Z.AI.
%%
%% Usage: rebar3 shell -eval "run_omega_with_zai:main([]), halt(0)."
%%        ZAI_API_KEY=your_key rebar3 shell -eval "run_omega_with_zai:main([]), halt(0)."
%%
%% Or: ./scripts/run_omega_with_zai.sh  (wrapper that runs the above)
main(_Args) ->
    run_omega_with_zai:main(_Args).
