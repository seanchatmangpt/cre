%% Generated regulation suite supervisor for Credit Union (Texas)
-module(f5_reg_credit_union_tx_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,  %% All regulations must pass
        intensity => 3,
        period => 60
    },

    %% Start validators for each regulation
    ChildSpecs = [
        #{
            id => tx_finance_code_validator,
            start => {f5_reg_credit_union_tx_tx_finance_code_validator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },
        #{
            id => ncua_part_701_validator,
            start => {f5_reg_credit_union_tx_ncua_part_701_validator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },
        #{
            id => fed_bsa_aml_validator,
            start => {f5_reg_credit_union_tx_fed_bsa_aml_validator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        }
    ],

    {{ok, {{SupFlags, ChildSpecs}}}}.
