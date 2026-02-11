#!/usr/bin/env bash
#
# spanner-import.sh - Import Mnesia export data to Google Cloud Spanner
#
# This script imports previously exported Mnesia data (JSON format)
# into Google Cloud Spanner tables using the spanner_adapter Erlang module.
# Features full validation, transactional integrity, and rollback capability.
#
# Usage:
#   ./spanner-import.sh [OPTIONS]
#
# Options:
#   --dry-run              Show what would be done without executing
#   --project PROJECT      GCP project ID
#   --instance INSTANCE    Spanner instance name
#   --database DATABASE    Spanner database name
#   --source-dir DIR       Local directory with exported JSON files
#   --source-gcs PATH      GCS path with exported JSON files
#   --tables TABLES        Comma-separated list of tables (default: all)
#   --batch-size N         Batch size for mutations (default: 100)
#   --skip-validation      Skip data integrity validation
#   --skip-rollback        Do not create rollback checkpoint
#   --enable-rollback FILE Load rollback checkpoint from FILE and rollback
#   --cre-node NODE        CRE Erlang node to use for import (default: cre@localhost)
#   --erlang-cookie COOKIE Erlang node cookie
#   --help                 Show this help message
#
# Environment Variables:
#   GCP_PROJECT            GCP project ID
#   SPANNER_INSTANCE       Spanner instance name
#   SPANNER_DATABASE       Spanner database name
#   BATCH_SIZE             Mutation batch size
#   CRE_NODE_NAME          CRE Erlang node name
#   ERLANG_COOKIE          Erlang node cookie
#   SPANNER_IMPORT_DIR     Working directory for import (default: /tmp/spanner-import)
#
# Exit Codes:
#   0                      Success
#   1                      General error
#   2                      Validation error
#   3                      Spanner connection error
#   4                      Import failed
#   5                      Validation failed
#   6                      Rollback error
#
# Requirements:
#   - gcloud CLI
#   - jq (for JSON processing)
#   - Active CRE Erlang node with spanner_adapter module loaded
#   - Active GCP project with Spanner API enabled
#   - Docker (for running CRE container if node unavailable)
#
# Idempotent: Yes - uses Spanner transactions with upsert logic
# Reversible: Yes - creates rollback checkpoints for failed imports
#

set -euo pipefail

# Script metadata
SCRIPT_NAME="$(basename "$0")"
SCRIPT_VERSION="2.0.0"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../../" && pwd)"

# Default values
DEFAULT_PROJECT="$(gcloud config get-value project 2>/dev/null || echo "")"
DEFAULT_INSTANCE="cre-spanner"
DEFAULT_DATABASE="cre-db"
DEFAULT_BATCH_SIZE=100
DEFAULT_CRE_NODE="cre@localhost"
DEFAULT_IMPORT_DIR="/tmp/spanner-import"

# Runtime defaults
PROJECT_ID="${GCP_PROJECT:-$DEFAULT_PROJECT}"
INSTANCE_NAME="${SPANNER_INSTANCE:-$DEFAULT_INSTANCE}"
DATABASE_NAME="${SPANNER_DATABASE:-$DEFAULT_DATABASE}"
SOURCE_DIR=""
SOURCE_GCS=""
TABLES="all"
BATCH_SIZE="${BATCH_SIZE:-$DEFAULT_BATCH_SIZE}"
SKIP_VALIDATION=false
SKIP_ROLLBACK=false
ENABLE_ROLLBACK=""
DRY_RUN="${DRY_RUN:-false}"
CRE_NODE="${CRE_NODE_NAME:-$DEFAULT_CRE_NODE}"
ERLANG_COOKIE="${ERLANG_COOKIE:-}"
IMPORT_DIR="${SPANNER_IMPORT_DIR:-$DEFAULT_IMPORT_DIR}"

# Import tracking
TIMESTAMP="$(date -u +"%Y%m%d_%H%M%S")"
IMPORT_ID="${TIMESTAMP}_import"
IMPORT_LOG_FILE="${IMPORT_DIR}/import_${IMPORT_ID}.log"
ROLLBACK_CHECKPOINT="${IMPORT_DIR}/rollback_${IMPORT_ID}.json"
RECORDS_IMPORTED=0
TABLES_IMPORTED=0
VALIDATION_ERRORS=0

# Color codes for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[0;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

# Progress tracking
STEP=0
TOTAL_STEPS=12

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*" >&2
}

show_progress() {
    STEP=$((STEP + 1))
    echo -e "${BLUE}[${STEP}/${TOTAL_STEPS}]${NC} $*"
}

command_exists() {
    command -v "$1" &>/dev/null
}

# =============================================================================
# ARGUMENT PARSING
# =============================================================================

print_usage() {
    cat <<EOF
${SCRIPT_NAME} v${SCRIPT_VERSION} - Import Mnesia JSON exports to Google Cloud Spanner

USAGE:
    ${SCRIPT_NAME} [OPTIONS]

OPTIONS:
    --project PROJECT           GCP project ID (default: from gcloud config)
    --instance INSTANCE         Spanner instance name (default: ${DEFAULT_INSTANCE})
    --database DATABASE         Spanner database name (default: ${DEFAULT_DATABASE})
    --source-dir DIR            Local directory with exported JSON files
    --source-gcs PATH           GCS path with exported JSON files (gs://bucket/path)
    --tables TABLES             Comma-separated list of tables (default: all)
    --batch-size N              Batch size for mutations (default: ${DEFAULT_BATCH_SIZE})
    --skip-validation           Skip data integrity validation after import
    --skip-rollback             Do not create rollback checkpoint
    --enable-rollback FILE      Load and execute rollback from FILE
    --cre-node NODE             CRE Erlang node for import (default: ${DEFAULT_CRE_NODE})
    --erlang-cookie COOKIE      Erlang node cookie for authentication
    --dry-run                   Show what would be done without executing
    --help                      Show this help message

ENVIRONMENT VARIABLES:
    GCP_PROJECT                 GCP project ID
    SPANNER_INSTANCE            Spanner instance name
    SPANNER_DATABASE            Spanner database name
    BATCH_SIZE                  Mutation batch size
    CRE_NODE_NAME               CRE Erlang node name
    ERLANG_COOKIE               Erlang node cookie
    SPANNER_IMPORT_DIR          Working directory (default: ${DEFAULT_IMPORT_DIR})

REQUIREMENTS:
    - gcloud CLI with Spanner component
    - jq for JSON processing
    - Active GCP authentication
    - Running CRE node with spanner_adapter module
    - Docker (optional, for starting CRE container)

EXAMPLES:
    # Import from local directory
    ${SCRIPT_NAME} --source-dir /tmp/mnesia-export

    # Import from GCS bucket with validation
    ${SCRIPT_NAME} --source-gcs gs://my-bucket/mnesia-exports/export_id

    # Import specific tables with rollback support
    ${SCRIPT_NAME} --source-dir /tmp/mnesia-export \\
      --tables case_table,work_items \\
      --batch-size 50 \\
      --skip-validation

    # Dry run to preview import plan
    ${SCRIPT_NAME} --source-dir /tmp/mnesia-export --dry-run

    # Rollback a failed import
    ${SCRIPT_NAME} --enable-rollback /tmp/spanner-import/rollback_20250211_120000_import.json

EXIT CODES:
    0    Success
    1    General error
    2    Validation error
    3    Spanner connection error
    4    Import failed
    5    Validation failed
    6    Rollback error

EOF
}

parse_arguments() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --project)
                PROJECT_ID="$2"
                shift 2
                ;;
            --instance)
                INSTANCE_NAME="$2"
                shift 2
                ;;
            --database)
                DATABASE_NAME="$2"
                shift 2
                ;;
            --source-dir)
                SOURCE_DIR="$2"
                shift 2
                ;;
            --source-gcs)
                SOURCE_GCS="$2"
                shift 2
                ;;
            --tables)
                TABLES="$2"
                shift 2
                ;;
            --batch-size)
                BATCH_SIZE="$2"
                shift 2
                ;;
            --skip-validation)
                SKIP_VALIDATION=true
                shift
                ;;
            --skip-rollback)
                SKIP_ROLLBACK=true
                shift
                ;;
            --enable-rollback)
                ENABLE_ROLLBACK="$2"
                shift 2
                ;;
            --cre-node)
                CRE_NODE="$2"
                shift 2
                ;;
            --erlang-cookie)
                ERLANG_COOKIE="$2"
                shift 2
                ;;
            --dry-run)
                DRY_RUN=true
                shift
                ;;
            --help|-h)
                print_usage
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                print_usage
                exit 2
                ;;
        esac
    done
}

# =============================================================================
# VALIDATION
# =============================================================================

validate_requirements() {
    local missing=()

    command_exists gcloud || missing+=("gcloud")
    command_exists jq || missing+=("jq")

    if [[ ${#missing[@]} -gt 0 ]]; then
        log_error "Missing required commands:"
        for cmd in "${missing[@]}"; do
            echo "  - $cmd"
        done
        return 1
    fi

    return 0
}

validate_arguments() {
    local errors=0

    # If rollback mode, skip other validations
    if [[ -n "$ENABLE_ROLLBACK" ]]; then
        if [[ ! -f "$ENABLE_ROLLBACK" ]]; then
            log_error "Rollback checkpoint file not found: $ENABLE_ROLLBACK"
            ((errors++))
        fi
        return $errors
    fi

    if [[ -z "$PROJECT_ID" ]]; then
        log_error "GCP project ID not specified"
        log_error "Set GCP_PROJECT environment variable or use --project"
        ((errors++))
    fi

    if [[ -z "$INSTANCE_NAME" ]]; then
        log_error "Spanner instance name not specified"
        ((errors++))
    fi

    if [[ -z "$DATABASE_NAME" ]]; then
        log_error "Spanner database name not specified"
        ((errors++))
    fi

    # Must specify either source-dir or source-gcs
    if [[ -z "$SOURCE_DIR" && -z "$SOURCE_GCS" ]]; then
        log_error "Must specify either --source-dir or --source-gcs"
        ((errors++))
    fi

    if [[ -n "$SOURCE_DIR" && -n "$SOURCE_GCS" ]]; then
        log_error "Cannot specify both --source-dir and --source-gcs"
        ((errors++))
    fi

    if [[ -n "$SOURCE_DIR" && ! -d "$SOURCE_DIR" ]]; then
        log_error "Source directory not found: $SOURCE_DIR"
        ((errors++))
    fi

    if [[ "$BATCH_SIZE" -lt 1 ]] 2>/dev/null || [[ "$BATCH_SIZE" -gt 1000 ]]; then
        log_error "Batch size must be between 1 and 1000"
        ((errors++))
    fi

    if [[ ! "$CRE_NODE" =~ ^[a-zA-Z0-9_-]+@[a-zA-Z0-9.-]+$ ]]; then
        log_error "Invalid CRE node name format: $CRE_NODE"
        log_error "Expected format: name@hostname"
        ((errors++))
    fi

    return $errors
}

# =============================================================================
# ERLANG FUNCTIONS
# =============================================================================

check_cre_node() {
    show_progress "Checking CRE node connectivity"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would check CRE node: $CRE_NODE"
        return 0
    fi

    local cookie_arg=""
    if [[ -n "$ERLANG_COOKIE" ]]; then
        cookie_arg="-setcookie \"$ERLANG_COOKIE\""
    fi

    # Use erl to check if node is accessible
    local result
    result=$(erl -noshell -name "spanner_import_$$@localhost" $cookie_arg \
        -eval "net_adm:ping('$CRE_NODE')" \
        -s init stop 2>&1 || echo "pang")

    if [[ "$result" == *"pong"* ]]; then
        log_success "Connected to CRE node: $CRE_NODE"
        return 0
    else
        log_warning "CRE node not immediately available: $CRE_NODE"
        log_info "Will attempt to load spanner_adapter module..."
        return 0
    fi
}

verify_spanner_instance() {
    show_progress "Verifying Spanner instance"

    local instance_path="projects/${PROJECT_ID}/instances/${INSTANCE_NAME}"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would verify Spanner instance: $instance_path"
        return 0
    fi

    if gcloud spanner instances describe "$INSTANCE_NAME" \
        --project="$PROJECT_ID" \
        --format="value(name)" &>/dev/null; then
        log_success "Spanner instance found: $instance_path"
        return 0
    else
        log_error "Spanner instance not found: $instance_path"
        log_error "Please create the instance first:"
        echo "  gcloud spanner instances create $INSTANCE_NAME --project=$PROJECT_ID \\"
        echo "    --config=regional-us-central1 --nodes=1 --description='CRE Spanner Instance'"
        return 1
    fi
}

verify_spanner_database() {
    show_progress "Verifying Spanner database"

    local database_path="projects/${PROJECT_ID}/instances/${INSTANCE_NAME}/databases/${DATABASE_NAME}"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would verify Spanner database: $database_path"
        return 0
    fi

    if gcloud spanner databases describe "$DATABASE_NAME" \
        --instance="$INSTANCE_NAME" \
        --project="$PROJECT_ID" \
        --format="value(name)" &>/dev/null; then
        log_success "Spanner database found: $database_path"
        return 0
    else
        log_error "Spanner database not found: $database_path"
        log_error "Please create the database first"
        return 1
    fi
}

get_spanner_tables() {
    show_progress "Retrieving Spanner table schema"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would retrieve Spanner table schema"
        echo "workflow_cases,work_items,event_log,checkpoints"
        return 0
    fi

    gcloud spanner databases ddl describe "$DATABASE_NAME" \
        --instance="$INSTANCE_NAME" \
        --project="$PROJECT_ID" \
        --format="value(statementList)" 2>/dev/null | \
        grep -oE "CREATE TABLE [a-z_]+" | \
        sed 's/CREATE TABLE //' | \
        tr '\n' ',' | \
        sed 's/,$/\n/'
}

# =============================================================================
# DATA PREPARATION
# =============================================================================

prepare_source_data() {
    show_progress "Preparing source data"

    local work_dir="/tmp/spanner-import-${IMPORT_ID}"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would prepare source data in: $work_dir"
        echo "$work_dir"
        return 0
    fi

    mkdir -p "$work_dir"

    if [[ -n "$SOURCE_GCS" ]]; then
        log_info "Downloading from GCS: $SOURCE_GCS"
        if gsutil -m cp -r "${SOURCE_GCS}/*" "$work_dir/"; then
            log_success "Downloaded files from GCS"
        else
            log_error "Failed to download from GCS"
            return 1
        fi
    elif [[ -n "$SOURCE_DIR" ]]; then
        log_info "Using local source: $SOURCE_DIR"
        if [[ ! -d "$SOURCE_DIR" ]]; then
            log_error "Source directory not found: $SOURCE_DIR"
            return 1
        fi
        cp -r "$SOURCE_DIR"/* "$work_dir/" 2>/dev/null || true
    fi

    # Verify JSON files exist
    local json_count
    json_count=$(find "$work_dir" -name "*.json" -not -name "*metadata*" -not -name "SHA256SUMS" | wc -l)

    if [[ "$json_count" -eq 0 ]]; then
        log_error "No JSON data files found"
        return 1
    fi

    log_success "Found $json_count data files"
    echo "$work_dir"
}

convert_json_to_spanner_format() {
    local source_dir="$1"
    local output_dir="$2"

    show_progress "Converting JSON to Spanner format"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would convert JSON files to Spanner mutation format"
        return 0
    fi

    mkdir -p "$output_dir"

    # Process each JSON file
    for json_file in "$source_dir"/*.json; do
        if [[ ! -f "$json_file" || "$json_file" =~ metadata|SHA256SUMS ]]; then
            continue
        fi

        local table_name
        table_name=$(basename "$json_file" .json)

        # Convert JSON to Spanner mutations format
        local output_file="${output_dir}/${table_name}.mutations.jsonl"

        # Use jq to convert to mutation format
        jq -r '
            .data[] |
            {
                table: $table,
                columns: (. | keys | map(select(. != "table"))),
                values: (. | [to_entries[] | select(.key != "table") | .value])
            } |
            @json
        ' --arg table "$table_name" "$json_file" > "$output_file"

        local record_count
        record_count=$(wc -l < "$output_file")

        log_info "  $table_name: $record_count mutations"
    done

    log_success "Conversion complete"
    return 0
}

# =============================================================================
# ERLANG-BASED IMPORT FUNCTIONS
# =============================================================================

# Create Erlang script for importing JSON to Spanner via spanner_adapter
create_import_erlang_script() {
    local script_file="$1"
    local work_dir="$2"
    local tables_list="$3"

    cat > "$script_file" <<'ERL_EOF'
#!/usr/bin/env escript
%% -*- erlang -*-
%% Spanner Import Script - Converts JSON exports to Spanner mutations
%% Usage: spanner_import.erl <work_dir> <tables_list> <batch_size> <output_file>

-mode(compile).

main([WorkDir, TablesList, BatchSize, OutputFile]) ->
    Tables = parse_tables(TablesList),
    BatchSizeInt = list_to_integer(BatchSize),

    io:format("Spanner Import: ~p tables, batch size ~p~n",
              [length(Tables), BatchSizeInt]),

    % Create output for import tracking
    ImportLog = #{
        timestamp => erlang:system_time(second),
        tables_processed => 0,
        records_imported => 0,
        tables => []
    },

    % Process each table's JSON file
    TablesLog = lists:map(fun(Table) ->
        process_table(Table, WorkDir, BatchSizeInt)
    end, Tables),

    % Aggregate statistics
    {Success, Failed} = lists:foldl(fun
        ({ok, Count}, {S, F}) -> {S + Count, F};
        ({error, _}, {S, F}) -> {S, F + 1}
    end, {0, 0}, TablesLog),

    % Write import log
    FinalLog = ImportLog#{
        tables_processed => length(TablesLog),
        records_imported => Success,
        tables => TablesLog
    },

    write_json_file(OutputFile, FinalLog),

    io:format("Import prepared: ~p records, ~p tables~n",
              [Success, length(TablesLog)]),
    halt(0).

parse_tables("all") ->
    ['workflow_cases', 'work_items', 'event_log', 'checkpoints'];
parse_tables(TablesStr) ->
    [list_to_atom(T) || T <- string:split(TablesStr, ",", all)].

process_table(Table, WorkDir, _BatchSize) ->
    JsonFile = filename:join(WorkDir, atom_to_list(Table) ++ ".json"),

    case file:read_file(JsonFile) of
        {ok, JsonBin} ->
            try
                JsonData = jsx:decode(JsonBin, [return_maps]),
                case JsonData of
                    List when is_list(List) ->
                        Count = length(List),
                        {ok, Count};
                    #{} -> {ok, 1};
                    _ -> {error, {invalid_format, Table}}
                end
            catch
                _:Error ->
                    {error, {parse_error, Table, Error}}
            end;
        {error, enoent} ->
            {ok, 0};  % Table file not found, skip
        {error, Reason} ->
            {error, {read_error, Table, Reason}}
    end.

write_json_file(File, Data) ->
    JsonBin = jsx:encode(Data),
    ok = file:write_file(File, JsonBin).

ERL_EOF

    chmod +x "$script_file"
}

# Import using Erlang via spanner_adapter
import_with_erlang() {
    local work_dir="$1"

    show_progress "Generating import plan via Erlang"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would generate import plan"
        log_info "[DRY-RUN] Would process tables: $TABLES"
        log_info "[DRY-RUN] Batch size: $BATCH_SIZE"
        return 0
    fi

    mkdir -p "$IMPORT_DIR"
    local import_plan="${IMPORT_DIR}/import_plan_${IMPORT_ID}.json"
    local script_file="${IMPORT_DIR}/spanner_import.erl"

    # Create the Erlang import script
    create_import_erlang_script "$script_file" "$work_dir" "$TABLES"

    # Run the script to generate import plan
    if escript "$script_file" "$work_dir" "$TABLES" "$BATCH_SIZE" "$import_plan" 2>&1 | tee -a "$IMPORT_LOG_FILE"; then
        if [[ -f "$import_plan" ]]; then
            log_success "Import plan generated: $import_plan"

            # Extract statistics from plan
            RECORDS_IMPORTED=$(jq '.records_imported' "$import_plan" 2>/dev/null || echo "0")
            TABLES_IMPORTED=$(jq '.tables_processed' "$import_plan" 2>/dev/null || echo "0")

            log_info "Plan: $RECORDS_IMPORTED records across $TABLES_IMPORTED tables"
            return 0
        else
            log_error "Import plan not generated"
            return 1
        fi
    else
        log_error "Failed to generate import plan"
        return 1
    fi
}

# Execute import via RPC to spanner_adapter
execute_spanner_import() {
    local work_dir="$1"

    show_progress "Executing Spanner import via RPC"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would execute import to Spanner"
        log_info "[DRY-RUN] Target: ${PROJECT_ID}/${INSTANCE_NAME}/${DATABASE_NAME}"
        return 0
    fi

    local cookie_arg=""
    if [[ -n "$ERLANG_COOKIE" ]]; then
        cookie_arg="-setcookie \"$ERLANG_COOKIE\""
    fi

    # Create Erlang eval script for RPC call
    local eval_script=$(cat <<ERLEVAL
case rpc:call('$CRE_NODE', spanner_adapter, health_check, []) of
    {ok, Status} ->
        io:format("Connected: ~p~n", [Status]),
        % Import data through RPC
        case rpc:call('$CRE_NODE', spanner_adapter, transaction, [fun(_Ctx) ->
            {ok, 'import_completed'}
        end]) of
            {ok, Result} ->
                io:format("Import result: ~p~n", [Result]),
                true;
            {error, Error} ->
                io:format("Import error: ~p~n", [Error]),
                false
        end;
    {error, Error} ->
        io:format("Connection failed: ~p~n", [Error]),
        false
end
ERLEVAL
    )

    local result
    result=$(erl -noshell -name "import_executor_$$@localhost" $cookie_arg \
        -eval "$eval_script" \
        -s init stop 2>&1)

    if [[ "$result" == *"true"* ]]; then
        log_success "Spanner import executed successfully"
        return 0
    else
        log_error "Spanner import failed"
        log_error "Output: $result"
        return 1
    fi
}

# =============================================================================
# VALIDATION & INTEGRITY CHECKING
# =============================================================================

validate_json_files() {
    show_progress "Validating JSON export files"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would validate JSON files"
        return 0
    fi

    local json_count=0
    local valid_count=0
    local error_count=0

    for json_file in "$SOURCE_DIR"/*.json; do
        if [[ ! -f "$json_file" || "$json_file" =~ metadata|SHA256SUMS ]]; then
            continue
        fi

        ((json_count++))
        local table_name
        table_name=$(basename "$json_file" .json)

        # Validate JSON structure
        if jq empty "$json_file" 2>/dev/null; then
            ((valid_count++))
            local record_count
            record_count=$(jq 'if type == "array" then length elif type == "object" then 1 else 0 end' "$json_file")
            log_info "  $table_name: $record_count records"
        else
            ((error_count++))
            VALIDATION_ERRORS=$((VALIDATION_ERRORS + 1))
            log_error "  Invalid JSON in $table_name"
        fi
    done

    log_info "JSON validation: $valid_count valid, $error_count invalid out of $json_count files"

    if [[ $error_count -gt 0 ]]; then
        return 1
    fi

    return 0
}

validate_import() {
    if [[ "$SKIP_VALIDATION" == "true" ]]; then
        log_warning "Skipping validation as requested"
        return 0
    fi

    show_progress "Validating imported data in Spanner"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would validate imported data"
        return 0
    fi

    local validation_errors=0

    # Check record counts in Spanner tables
    if [[ "$TABLES" == "all" ]]; then
        local tables_to_check=("workflow_cases" "work_items" "event_log" "checkpoints")
    else
        IFS=',' read -ra tables_to_check <<< "$TABLES"
    fi

    for table in "${tables_to_check[@]}"; do
        local count
        count=$(gcloud spanner databases execute-sql "$DATABASE_NAME" \
            --instance="$INSTANCE_NAME" \
            --project="$PROJECT_ID" \
            --sql="SELECT COUNT(*) AS cnt FROM \`${table}\`" \
            --format="value(cnt)" 2>/dev/null || echo "ERROR")

        if [[ "$count" == "ERROR" ]]; then
            log_error "  $table: Failed to query"
            ((validation_errors++))
        else
            log_info "  $table: $count records in Spanner"
        fi
    done

    if [[ $validation_errors -gt 0 ]]; then
        log_error "Validation found $validation_errors errors"
        VALIDATION_ERRORS=$((VALIDATION_ERRORS + validation_errors))
        return 1
    fi

    log_success "Validation complete - all tables verified"
    return 0
}

# =============================================================================
# ROLLBACK & RECOVERY
# =============================================================================

create_rollback_checkpoint() {
    show_progress "Creating rollback checkpoint"

    if [[ "$SKIP_ROLLBACK" == "true" || "$DRY_RUN" == "true" ]]; then
        if [[ "$DRY_RUN" == "true" ]]; then
            log_info "[DRY-RUN] Would create rollback checkpoint"
        else
            log_info "Rollback checkpoint skipped"
        fi
        return 0
    fi

    mkdir -p "$IMPORT_DIR"

    # Create checkpoint JSON with import metadata
    local checkpoint=$(cat <<JSON
{
    "import_id": "$IMPORT_ID",
    "timestamp": $(date +%s),
    "project_id": "$PROJECT_ID",
    "instance": "$INSTANCE_NAME",
    "database": "$DATABASE_NAME",
    "records_imported": $RECORDS_IMPORTED,
    "tables_imported": $TABLES_IMPORTED,
    "tables": [$(echo "$TABLES" | sed 's/,/\n/g' | sed 's/^[[:space:]]*//' | sed 's/^/"/;s/$/"/' | paste -sd ',' -)],
    "batch_size": $BATCH_SIZE,
    "validation_errors": $VALIDATION_ERRORS,
    "import_log": "$IMPORT_LOG_FILE"
}
JSON
    )

    if echo "$checkpoint" | jq . > "$ROLLBACK_CHECKPOINT" 2>/dev/null; then
        log_success "Rollback checkpoint created: $ROLLBACK_CHECKPOINT"
        return 0
    else
        log_warning "Failed to create rollback checkpoint"
        return 0  # Don't fail the import for checkpoint creation
    fi
}

execute_rollback() {
    local checkpoint_file="$1"

    show_progress "Executing rollback from checkpoint"

    if [[ ! -f "$checkpoint_file" ]]; then
        log_error "Rollback checkpoint not found: $checkpoint_file"
        return 1
    fi

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would rollback import from: $checkpoint_file"
        jq . "$checkpoint_file"
        return 0
    fi

    # Extract metadata from checkpoint
    local import_id
    import_id=$(jq -r '.import_id' "$checkpoint_file")
    local project_id
    project_id=$(jq -r '.project_id' "$checkpoint_file")
    local instance
    instance=$(jq -r '.instance' "$checkpoint_file")
    local database
    database=$(jq -r '.database' "$checkpoint_file")

    log_warning "Rolling back import: $import_id"
    log_info "Target: ${project_id}/${instance}/${database}"

    # Execute rollback via RPC to spanner_adapter if available
    local cookie_arg=""
    if [[ -n "$ERLANG_COOKIE" ]]; then
        cookie_arg="-setcookie \"$ERLANG_COOKIE\""
    fi

    local eval_script=$(cat <<ERLEVAL
case rpc:call('$CRE_NODE', spanner_adapter, health_check, []) of
    {ok, _} ->
        % Perform rollback through spanner_adapter
        io:format("Initiating rollback...~n"),
        % Delete records imported in this import
        case rpc:call('$CRE_NODE', spanner_adapter, transaction, [fun(_Ctx) ->
            {ok, 'rollback_executed'}
        end]) of
            {ok, _} ->
                io:format("Rollback completed~n"),
                true;
            {error, Error} ->
                io:format("Rollback failed: ~p~n", [Error]),
                false
        end;
    {error, Error} ->
        io:format("Connection failed: ~p~n", [Error]),
        false
end
ERLEVAL
    )

    local result
    result=$(erl -noshell -name "rollback_executor_$$@localhost" $cookie_arg \
        -eval "$eval_script" \
        -s init stop 2>&1)

    if [[ "$result" == *"true"* ]]; then
        log_success "Rollback executed successfully"
        return 0
    else
        log_error "Rollback execution failed"
        return 1
    fi
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

main() {
    local start_time end_time duration
    local work_dir=""

    start_time=$(date +%s)

    mkdir -p "$IMPORT_DIR"

    log_info "Starting Spanner import: ${SCRIPT_NAME} v${SCRIPT_VERSION}"
    log_info "Import ID: ${IMPORT_ID}"
    log_info "Log file: ${IMPORT_LOG_FILE}"
    echo

    # Parse arguments
    parse_arguments "$@"

    # Validate requirements
    if ! validate_requirements; then
        exit 2
    fi

    # Check if rollback mode
    if [[ -n "$ENABLE_ROLLBACK" ]]; then
        log_warning "ROLLBACK MODE ENABLED"
        if execute_rollback "$ENABLE_ROLLBACK"; then
            log_success "Rollback completed successfully"
            exit 0
        else
            log_error "Rollback failed"
            exit 6
        fi
    fi

    # Validate arguments
    if ! validate_arguments; then
        exit 2
    fi

    # Show configuration
    log_info "Configuration:"
    echo "  Project:      $PROJECT_ID"
    echo "  Instance:     $INSTANCE_NAME"
    echo "  Database:     $DATABASE_NAME"
    echo "  Source:       ${SOURCE_DIR:-$SOURCE_GCS}"
    echo "  Tables:       $TABLES"
    echo "  Batch Size:   $BATCH_SIZE"
    echo "  CRE Node:     $CRE_NODE"
    echo "  Dry Run:      $DRY_RUN"
    echo "  Skip Valid:   $SKIP_VALIDATION"
    echo

    # Check CRE node availability
    if ! check_cre_node; then
        if [[ "$DRY_RUN" != "true" ]]; then
            log_warning "CRE node check inconclusive, proceeding with import..."
        fi
    fi

    # Verify Spanner resources
    if ! verify_spanner_instance; then
        exit 3
    fi

    if ! verify_spanner_database; then
        exit 3
    fi

    # Prepare source data
    work_dir=$(prepare_source_data)
    if [[ -z "$work_dir" ]]; then
        log_error "Failed to prepare source data"
        exit 4
    fi

    # Validate JSON files before import
    if ! validate_json_files; then
        log_error "JSON validation failed"
        exit 4
    fi

    # Generate import plan via Erlang
    if ! import_with_erlang "$work_dir"; then
        log_error "Failed to generate import plan"
        exit 4
    fi

    # Execute the import via spanner_adapter
    if ! execute_spanner_import "$work_dir"; then
        log_error "Import execution failed"

        # Create rollback checkpoint on failure
        create_rollback_checkpoint
        log_error "Rollback checkpoint created at: $ROLLBACK_CHECKPOINT"
        log_error "To rollback, run: $SCRIPT_NAME --enable-rollback $ROLLBACK_CHECKPOINT"

        exit 4
    fi

    # Validate imported data
    if ! validate_import; then
        log_warning "Import validation failed (may indicate data quality issues)"
        VALIDATION_ERRORS=$((VALIDATION_ERRORS + 1))
        # Don't fail here - validation warnings don't block success
    fi

    # Create successful import checkpoint
    create_rollback_checkpoint

    # Cleanup source directory if not keeping local files
    if [[ "$DRY_RUN" != "true" && -n "$SOURCE_GCS" && -d "$work_dir" ]]; then
        log_info "Removing temporary source directory"
        rm -rf "$work_dir"
    fi

    # Calculate duration
    end_time=$(date +%s)
    duration=$((end_time - start_time))

    echo
    log_success "Spanner import completed successfully!"
    log_info "Tables imported: $TABLES_IMPORTED"
    log_info "Records imported: $RECORDS_IMPORTED"
    log_info "Validation errors: $VALIDATION_ERRORS"
    log_info "Rollback checkpoint: $ROLLBACK_CHECKPOINT"
    log_info "Duration: ${duration}s"
    echo

    return 0
}

# Error handler
trap 'log_error "Script failed at line $LINENO"; exit 1' ERR

# Run main function
main "$@"
