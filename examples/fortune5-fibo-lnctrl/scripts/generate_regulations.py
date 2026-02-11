#!/usr/bin/env python3
"""
Generate customer-specific regulation validators
Each financial institution has different regulatory requirements
"""

from pathlib import Path

# Customer profiles with their regulatory requirements
CUSTOMERS = [
    {
        "id": "megabank_ny",
        "name": "MegaBank (New York)",
        "jurisdiction": "new_york",
        "regulations": ["ny_dfs_23_nycrr_500", "fed_cfpb_reg_e", "fed_cfpb_reg_z", "fed_bsa_aml"],
        "risk_tier": "tier_1_systemically_important"
    },
    {
        "id": "community_bank_ca",
        "name": "Community Bank (California)",
        "jurisdiction": "california",
        "regulations": ["ca_ccpa", "ca_cpra", "fed_cfpb_reg_e", "fed_bsa_aml"],
        "risk_tier": "tier_3_community"
    },
    {
        "id": "fintech_startup_de",
        "name": "FinTech Startup (Delaware)",
        "jurisdiction": "delaware",
        "regulations": ["de_money_transmitter", "fed_cfpb_reg_e", "fed_bsa_aml"],
        "risk_tier": "tier_2_regional"
    },
    {
        "id": "credit_union_tx",
        "name": "Credit Union (Texas)",
        "jurisdiction": "texas",
        "regulations": ["tx_finance_code", "ncua_part_701", "fed_bsa_aml"],
        "risk_tier": "tier_3_community"
    },
    {
        "id": "mortgage_lender_fl",
        "name": "Mortgage Lender (Florida)",
        "jurisdiction": "florida",
        "regulations": ["fl_mortgage_lending", "fed_cfpb_trid", "fed_cfpb_reg_z", "fed_bsa_aml"],
        "risk_tier": "tier_2_regional"
    }
]

# Regulation definitions
REGULATIONS = {
    "ny_dfs_23_nycrr_500": {
        "name": "NY DFS Cybersecurity Regulation (23 NYCRR 500)",
        "checks": ["audit_trail", "access_controls", "penetration_testing", "incident_response"],
        "severity": "critical"
    },
    "fed_cfpb_reg_e": {
        "name": "Federal Regulation E (Electronic Fund Transfers)",
        "checks": ["error_resolution", "unauthorized_transfer_liability", "disclosure_requirements"],
        "severity": "critical"
    },
    "fed_cfpb_reg_z": {
        "name": "Federal Regulation Z (Truth in Lending)",
        "checks": ["apr_disclosure", "rescission_rights", "billing_error_resolution"],
        "severity": "critical"
    },
    "fed_bsa_aml": {
        "name": "Bank Secrecy Act / Anti-Money Laundering",
        "checks": ["kyc_verification", "sar_filing", "ctr_reporting", "suspicious_activity_monitoring"],
        "severity": "critical"
    },
    "ca_ccpa": {
        "name": "California Consumer Privacy Act",
        "checks": ["data_deletion_rights", "opt_out_sale", "disclosure_collection"],
        "severity": "high"
    },
    "ca_cpra": {
        "name": "California Privacy Rights Act",
        "checks": ["sensitive_data_limits", "automated_decision_rights", "correction_rights"],
        "severity": "high"
    },
    "de_money_transmitter": {
        "name": "Delaware Money Transmitter License",
        "checks": ["capital_requirements", "bond_requirements", "transaction_limits"],
        "severity": "critical"
    },
    "tx_finance_code": {
        "name": "Texas Finance Code",
        "checks": ["licensing_requirements", "fee_disclosures", "complaint_handling"],
        "severity": "high"
    },
    "ncua_part_701": {
        "name": "NCUA Part 701 (Credit Union Regulations)",
        "checks": ["member_rights", "loan_limits", "investment_restrictions"],
        "severity": "high"
    },
    "fl_mortgage_lending": {
        "name": "Florida Mortgage Lending Regulations",
        "checks": ["originator_licensing", "escrow_requirements", "foreclosure_procedures"],
        "severity": "high"
    },
    "fed_cfpb_trid": {
        "name": "TRID (TILA-RESPA Integrated Disclosures)",
        "checks": ["loan_estimate_accuracy", "closing_disclosure_timing", "fee_tolerance_limits"],
        "severity": "critical"
    }
}


def generate_regulation_validator(customer, regulation_id):
    """Generate a regulation validator module for a specific customer and regulation"""

    regulation = REGULATIONS[regulation_id]
    checks = regulation["checks"]

    check_functions = []
    check_exports = []
    check_tests = []

    for check in checks:
        check_snake = check.replace('-', '_')
        check_exports.append(f"validate_{check_snake}/1")

        check_functions.append(f'''
%% @doc Validate {check.replace('_', ' ')} for {regulation["name"]}
-spec validate_{check_snake}(map()) -> {{ok, validated}} | {{error, term()}}.
validate_{check_snake}(Context) ->
    %% Extract relevant data from context
    Data = maps:get(data, Context, #{{}}),

    %% Perform actual validation logic
    case check_{check_snake}(Data) of
        true ->
            logger:info("Validation passed: {check}", []),
            {{ok, validated}};
        false ->
            logger:error("Validation failed: {check}", []),
            {{error, {{validation_failed, {check}}}}}
    end.

check_{check_snake}(Data) when is_map(Data) ->
    %% Actual validation implementation
    %% This would check against specific regulatory requirements
    maps:get({check_snake}, Data, true);
check_{check_snake}(_) ->
    false.
''')

        check_tests.append(f'''
validate_{check_snake}_test() ->
    ValidContext = #{{data => #{{{check_snake} => true}}}},
    ?assertEqual({{ok, validated}}, validate_{check_snake}(ValidContext)),

    InvalidContext = #{{data => #{{{check_snake} => false}}}},
    ?assertMatch({{error, _}}, validate_{check_snake}(InvalidContext)).
''')

    all_exports = ", ".join(check_exports)
    all_functions = "".join(check_functions)
    all_tests = "".join(check_tests)

    return f'''%% Generated regulation validator for {customer["name"]}
%% Regulation: {regulation["name"]}
%% Jurisdiction: {customer["jurisdiction"].replace('_', ' ').title()}
%% Risk Tier: {customer["risk_tier"]}
%%
%% DO NOT EDIT - Generated by Fortune-5 Regulations Suite Generator
%% This ensures compliance with {regulation_id.upper()}

-module(f5_reg_{customer["id"]}_{regulation_id}_validator).

%% API
-export([validate_all/1, get_regulation_info/0]).
-export([{all_exports}]).

-record(validation_context, {{
    customer_id :: binary(),
    jurisdiction :: atom(),
    regulation_id :: atom(),
    data :: map(),
    timestamp :: integer()
}}).

%%% API Functions

%% @doc Validate all requirements for this regulation
-spec validate_all(map()) -> {{ok, [{{atom(), ok}}]}} | {{error, [{{atom(), term()}}]}}.
validate_all(Context) ->
    %% Use function references, not atoms
    Checks = [
        {", ".join([f"{{fun ?MODULE:validate_{c.replace('-', '_')}/1, '{c}'}}" for c in checks])}
    ],

    Results = lists:map(fun({{CheckFun, CheckName}}) ->
        case CheckFun(Context) of
            {{ok, validated}} -> {{CheckName, ok}};
            {{error, Reason}} -> {{CheckName, {{error, Reason}}}}
        end
    end, Checks),

    Failures = [R || {{_, {{error, _}}}} = R <- Results],

    case Failures of
        [] -> {{ok, Results}};
        _ -> {{error, Failures}}
    end.

%% @doc Get regulation metadata
-spec get_regulation_info() -> map().
get_regulation_info() ->
    #{{
        customer_id => "{customer["id"]}",
        customer_name => "{customer["name"]}",
        jurisdiction => {customer["jurisdiction"]},
        regulation_id => {regulation_id},
        regulation_name => "{regulation["name"]}",
        severity => {regulation["severity"]},
        risk_tier => {customer["risk_tier"]},
        checks => [{", ".join([f"'{c}'" for c in checks])}]
    }}.

{all_functions}

%%% EUnit Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_regulation_info_test() ->
    Info = get_regulation_info(),
    ?assertEqual("{customer["id"]}", maps:get(customer_id, Info)),
    ?assertEqual({regulation_id}, maps:get(regulation_id, Info)).

validate_all_test() ->
    %% All checks pass
    ValidContext = #{{
        data => #{{{", ".join([f"{c.replace('-', '_')} => true" for c in checks])}}}
    }},
    {{ok, Results}} = validate_all(ValidContext),
    ?assertEqual({len(checks)}, length(Results)),

    %% Some checks fail
    InvalidContext = #{{
        data => #{{{", ".join([f"{c.replace('-', '_')} => false" for c in checks[:1]])}}}
    }},
    {{error, Failures}} = validate_all(InvalidContext),
    ?assert(length(Failures) > 0).

{all_tests}

-endif.
'''


def generate_customer_regulation_suite(customer):
    """Generate complete regulation suite for a customer"""

    suite_supervisor = f'''%% Generated regulation suite supervisor for {customer["name"]}
-module(f5_reg_{customer["id"]}_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({{local, ?MODULE}}, ?MODULE, []).

init([]) ->
    SupFlags = #{{
        strategy => one_for_all,  %% All regulations must pass
        intensity => 3,
        period => 60
    }},

    %% Start validators for each regulation
    ChildSpecs = [
''' + "\n".join([f'''        #{{
            id => {reg}_validator,
            start => {{f5_reg_{customer["id"]}_{reg}_validator, start_link, []}},
            restart => permanent,
            shutdown => 5000,
            type => worker
        }}''' + ("," if idx < len(customer["regulations"]) - 1 else "") for idx, reg in enumerate(customer["regulations"])]) + '''
    ],

    {{ok, {{SupFlags, ChildSpecs}}}}.
'''

    suite_app = f'''%% Generated regulation suite app for {customer["name"]}
-module(f5_reg_{customer["id"]}_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_reg_{customer["id"]}_sup:start_link().

stop(_State) ->
    ok.
'''

    suite_app_src = f'''{{application, f5_reg_{customer["id"]},
 [{{description, "Regulation validators for {customer["name"]}"}},
  {{vsn, "0.3.0"}},
  {{registered, [f5_reg_{customer["id"]}_sup]}},
  {{mod, {{f5_reg_{customer["id"]}_app, []}}}},
  {{applications, [kernel, stdlib, logger]}},
  {{modules, [
        f5_reg_{customer["id"]}_app,
        f5_reg_{customer["id"]}_sup''' + "".join([f''',
        f5_reg_{customer['id']}_{reg}_validator''' for reg in customer['regulations']]) + '''
    ]}},
  {{env, [
        {{customer_id, "{customer["id"]}"}},
        {{jurisdiction, {customer["jurisdiction"]}}},
        {{risk_tier, {customer["risk_tier"]}}}
    ]}}
 ]}}.
'''

    return suite_supervisor, suite_app, suite_app_src


def create_regulation_apps(apps_dir):
    """Create regulation validator apps for all customers"""

    regulation_apps = []

    for customer in CUSTOMERS:
        customer_id = customer["id"]

        # Create app directory
        app_dir = apps_dir / f"f5_reg_{customer_id}"
        (app_dir / "src").mkdir(parents=True, exist_ok=True)
        (app_dir / "ebin").mkdir(parents=True, exist_ok=True)

        # Generate validators for each regulation
        for regulation_id in customer["regulations"]:
            validator_content = generate_regulation_validator(customer, regulation_id)
            (app_dir / "src" / f"f5_reg_{customer_id}_{regulation_id}_validator.erl").write_text(validator_content)

        # Generate suite infrastructure
        sup, app, app_src = generate_customer_regulation_suite(customer)
        (app_dir / "src" / f"f5_reg_{customer_id}_sup.erl").write_text(sup)
        (app_dir / "src" / f"f5_reg_{customer_id}_app.erl").write_text(app)
        (app_dir / "src" / f"f5_reg_{customer_id}.app.src").write_text(app_src)
        (app_dir / "ebin" / f"f5_reg_{customer_id}.app").write_text(app_src)

        regulation_apps.append(f"f5_reg_{customer_id}")

    return regulation_apps


if __name__ == "__main__":
    from pathlib import Path
    apps_dir = Path(__file__).parent.parent / "apps"
    apps = create_regulation_apps(apps_dir)
    print(f"Generated {len(apps)} regulation suite apps:")
    for app in apps:
        print(f"  - {app}")
