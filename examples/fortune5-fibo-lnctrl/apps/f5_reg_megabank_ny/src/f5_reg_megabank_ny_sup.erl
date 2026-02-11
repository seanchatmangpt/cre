%% Generated regulation suite supervisor for MegaBank (New York)
-module(f5_reg_megabank_ny_sup).
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
            id => ny_dfs_23_nycrr_500_validator,
            start => {f5_reg_megabank_ny_ny_dfs_23_nycrr_500_validator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },
        #{
            id => fed_cfpb_reg_e_validator,
            start => {f5_reg_megabank_ny_fed_cfpb_reg_e_validator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },
        #{
            id => fed_cfpb_reg_z_validator,
            start => {f5_reg_megabank_ny_fed_cfpb_reg_z_validator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },
        #{
            id => fed_bsa_aml_validator,
            start => {f5_reg_megabank_ny_fed_bsa_aml_validator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        }
    ],

    {{ok, {{SupFlags, ChildSpecs}}}}.
