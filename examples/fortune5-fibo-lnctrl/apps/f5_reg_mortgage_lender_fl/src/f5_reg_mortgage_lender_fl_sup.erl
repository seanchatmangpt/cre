%% Generated regulation suite supervisor for Mortgage Lender (Florida)
-module(f5_reg_mortgage_lender_fl_sup).
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
            id => fl_mortgage_lending_validator,
            start => {f5_reg_mortgage_lender_fl_fl_mortgage_lending_validator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },
        #{
            id => fed_cfpb_trid_validator,
            start => {f5_reg_mortgage_lender_fl_fed_cfpb_trid_validator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },
        #{
            id => fed_cfpb_reg_z_validator,
            start => {f5_reg_mortgage_lender_fl_fed_cfpb_reg_z_validator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },
        #{
            id => fed_bsa_aml_validator,
            start => {f5_reg_mortgage_lender_fl_fed_bsa_aml_validator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        }
    ],

    {{ok, {{SupFlags, ChildSpecs}}}}.
