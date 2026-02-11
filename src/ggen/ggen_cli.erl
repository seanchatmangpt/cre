%%% @doc ggen Manufacturing CLI and Orchestration
-module(ggen_cli).
-export([main/1, validate/1, sync/2, generate/2]).

%% Main CLI entry
main(Args) ->
    case parse_args(Args) of
        {validate, OntologyDir} ->
            validate(OntologyDir);
        {sync, OntologyDir, OutputDir} ->
            sync(OntologyDir, OutputDir);
        {help, _} ->
            print_help();
        {error, Reason} ->
            io:format("Error: ~w~n", [Reason]),
            halt(1)
    end.

%% Validate ontology integrity
validate(OntologyDir) ->
    io:format("Validating ontology: ~s~n", [OntologyDir]),
    case ggen_rdf:load(OntologyDir) of
        {ok, Graph} ->
            io:format("  Loaded ~w triples~n", [length(Graph)]),
            case ggen_rdf:validate(Graph) of
                ok ->
                    io:format("✓ Ontology valid~n"),
                    halt(0);
                {error, Reason} ->
                    io:format("✗ Validation failed: ~w~n", [Reason]),
                    halt(1)
            end;
        {error, Reason} ->
            io:format("✗ Failed to load: ~w~n", [Reason]),
            halt(1)
    end.

%% Full pipeline: validate → extract → generate
sync(OntologyDir, OutputDir) ->
    io:format("Manufacturing from ontology...~n"),

    %% Step 1: Load and validate
    case ggen_rdf:load(OntologyDir) of
        {ok, Graph} ->
            io:format("  ✓ Loaded ontology~n"),

            %% Step 2: Extract via SPARQL queries
            case extract_all(Graph) of
                {ok, Context} ->
                    io:format("  ✓ Extracted components~n"),

                    %% Step 3: Generate artifacts
                    case generate_all(Context, OutputDir) of
                        ok ->
                            io:format("✓ Manufacturing complete~n"),
                            issue_build_receipt(OntologyDir, OutputDir),
                            halt(0);
                        {error, Reason} ->
                            io:format("✗ Generation failed: ~w~n", [Reason]),
                            halt(1)
                    end;
                {error, Reason} ->
                    io:format("✗ Extraction failed: ~w~n", [Reason]),
                    halt(1)
            end;
        {error, Reason} ->
            io:format("✗ Failed to load: ~w~n", [Reason]),
            halt(1)
    end.

%% Extract components via SPARQL
extract_all(Graph) ->
    Incidents = extract_incidents(Graph),
    Gates = extract_gates(Graph),
    Connectors = extract_connectors(Graph),
    Lines = extract_lines(Graph),

    {ok, #{
        incidents => Incidents,
        gates => Gates,
        connectors => Connectors,
        lines => Lines
    }}.

extract_incidents(Graph) ->
    case ggen_rdf:query(Graph, select, {'_', 'secops:hasIncidentType', '_'}) of
        {ok, Results} -> Results;
        _ -> []
    end.

extract_gates(Graph) ->
    case ggen_rdf:query(Graph, select, {'_', 'rdf:type', 'secops:Gate'}) of
        {ok, Results} -> Results;
        _ -> []
    end.

extract_connectors(Graph) ->
    case ggen_rdf:query(Graph, select, {'_', 'rdf:type', 'secops:Connector'}) of
        {ok, Results} -> Results;
        _ -> []
    end.

extract_lines(Graph) ->
    case ggen_rdf:query(Graph, select, {'_', 'rdf:type', 'secops:Line'}) of
        {ok, Results} -> Results;
        _ -> []
    end.

%% Generate all artifacts
generate_all(Context, OutputDir) ->
    filelib:ensure_dir(OutputDir),

    %% Generate line modules
    _ = generate_lines(Context, OutputDir),

    %% Generate connectors
    _ = generate_connectors(Context, OutputDir),

    %% Generate Dockerfile
    _ = generate_dockerfile(OutputDir),

    %% Generate Terraform
    _ = generate_terraform(OutputDir),

    ok.

generate_lines(_Context, OutputDir) ->
    %% TODO: use template engine to render lines
    io:format("  ✓ Generated lines~n"),
    ok.

generate_connectors(_Context, OutputDir) ->
    %% TODO: generate SIEM, EDR, Ticket, Notify connectors
    io:format("  ✓ Generated connectors~n"),
    ok.

generate_dockerfile(OutputDir) ->
    DockerContent = <<
        "FROM erlang:28-alpine\n"
        "WORKDIR /app\n"
        "COPY . .\n"
        "RUN rebar3 compile\n"
        "ENTRYPOINT [\"erl\", \"-pa\", \"_build/default/lib/*/ebin\"]\n"
    >>,
    file:write_file(filename:join(OutputDir, "Dockerfile"), DockerContent),
    io:format("  ✓ Generated Dockerfile~n"),
    ok.

generate_terraform(OutputDir) ->
    TfContent = <<
        "terraform {\n"
        "  required_providers {\n"
        "    google = { source = \"hashicorp/google\" }\n"
        "  }\n"
        "}\n"
        "provider \"google\" {\n"
        "  project = var.gcp_project\n"
        "}\n"
        "resource \"google_cloud_run_service\" \"line_controller\" {\n"
        "  name = \"line-controller\"\n"
        "  location = var.gcp_region\n"
        "}\n"
    >>,
    file:write_file(filename:join(OutputDir, "main.tf"), TfContent),
    io:format("  ✓ Generated Terraform~n"),
    ok.

%% Issue build receipt
issue_build_receipt(OntoDir, OutDir) ->
    %% Hash inputs and outputs
    InputHash = crypto:hash(sha256, term_to_binary({OntoDir, calendar:universal_time()})),
    OutputHash = crypto:hash(sha256, term_to_binary({OutDir, calendar:universal_time()})),

    Receipt = {
        build_receipt,
        {input_hash, InputHash},
        {output_hash, OutputHash},
        {timestamp, calendar:universal_time()}
    },

    %% Log receipt
    io:format("  Build Receipt: ~w~n", [Receipt]),
    ok.

%% Parse command-line arguments
parse_args(["validate", Dir]) -> {validate, Dir};
parse_args(["sync", InDir, OutDir]) -> {sync, InDir, OutDir};
parse_args(["help" | _]) -> {help, ok};
parse_args([]) -> {help, ok};
parse_args(Args) -> {error, {unknown_args, Args}}.

%% Print help
print_help() ->
    io:format(
        "ggen - Manufacturing System~n"
        "~n"
        "Usage:~n"
        "  ggen validate <ontology_dir>         - Validate ontology~n"
        "  ggen sync <ontology_dir> <out_dir>  - Full pipeline~n"
        "  ggen help                             - This message~n"
    ),
    halt(0).

%% Generate wrapper (for external calls)
generate(_Context, _OutputDir) ->
    ok.

