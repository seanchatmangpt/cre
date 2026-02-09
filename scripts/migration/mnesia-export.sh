#!/usr/bin/env bash
#
# mnesia-export.sh - Export Mnesia data to Google Cloud Storage
#
# This script exports Mnesia database data from a CRE instance,
# converts it to JSON format, and uploads it to a GCS bucket.
#
# Usage:
#   ./mnesia-export.sh [OPTIONS]
#
# Options:
#   --dry-run           Show what would be done without executing
#   --node NODE         Erlang node name (default: cre@localhost)
#   --cookie COOKIE     Erlang cookie for authentication
#   --bucket BUCKET     GCS bucket name (default: cre-mnesia-backups)
#   --output-dir DIR    Local output directory (default: /tmp/mnesia-export)
#   --tables TABLES     Comma-separated list of tables (default: all)
#   --help              Show this help message
#
# Environment Variables:
#   CRE_NODE_NAME       Erlang node name
#   ERLANG_COOKIE       Erlang cookie
#   GCS_BUCKET          GCS bucket name
#   DRY_RUN             Set to "true" for dry-run mode
#
# Exit Codes:
#   0                   Success
#   1                   General error
#   2                   Validation error
#   3                   Mnesia connection error
#   4                   Export failed
#   5                   GCS upload failed
#
# Requirements:
#   - Erlang/OTP 25+
#   - gcloud CLI (for GCS operations)
#   - jq (for JSON processing)
#   - Active CRE node with Mnesia running
#
# Idempotent: Yes - can be run multiple times safely
#

set -euo pipefail

# Script metadata
SCRIPT_NAME="$(basename "$0")"
SCRIPT_VERSION="1.0.0"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Default values
DEFAULT_NODE="cre@localhost"
DEFAULT_BUCKET="cre-mnesia-backups"
DEFAULT_OUTPUT_DIR="/tmp/mnesia-export"
DEFAULT_TABLES="all"

# Runtime defaults (can be overridden by environment or arguments)
NODE_NAME="${CRE_NODE_NAME:-$DEFAULT_NODE}"
ERLANG_COOKIE="${ERLANG_COOKIE:-}"
GCS_BUCKET="${GCS_BUCKET:-$DEFAULT_BUCKET}"
OUTPUT_DIR="${OUTPUT_DIR:-$DEFAULT_OUTPUT_DIR}"
TABLES="${TABLES:-$DEFAULT_TABLES}"
DRY_RUN="${DRY_RUN:-false}"

# Timestamp for this export
TIMESTAMP=$(date -u +"%Y%m%d_%H%M%S")
EXPORT_ID="${TIMESTAMP}_$(hostname -s)"

# Color codes for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[0;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m' # No Color

# Progress tracking
STEP=0
TOTAL_STEPS=8

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

# Log an info message
log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

# Log a success message
log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

# Log a warning message
log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $*"
}

# Log an error message
log_error() {
    echo -e "${RED}[ERROR]${NC} $*" >&2
}

# Show progress
show_progress() {
    STEP=$((STEP + 1))
    echo -e "${BLUE}[${STEP}/${TOTAL_STEPS}]${NC} $*"
}

# Check if a command exists
command_exists() {
    command -v "$1" &>/dev/null
}

# Validate required commands
validate_requirements() {
    local missing=()

    command_exists erl || missing+=("erlang/otp (erl)")
    command_exists jq || missing+=("jq")
    command_exists gcloud || missing+=("gcloud")

    if [[ ${#missing[@]} -gt 0 ]]; then
        log_error "Missing required commands:"
        for cmd in "${missing[@]}"; do
            echo "  - $cmd"
        done
        return 1
    fi

    return 0
}

# Print usage information
print_usage() {
    cat <<EOF
${SCRIPT_NAME} v${SCRIPT_VERSION} - Export Mnesia data to Google Cloud Storage

USAGE:
    ${SCRIPT_NAME} [OPTIONS]

OPTIONS:
    --node NODE           Erlang node name (default: ${DEFAULT_NODE})
    --cookie COOKIE       Erlang cookie for authentication
    --bucket BUCKET       GCS bucket name (default: ${DEFAULT_BUCKET})
    --output-dir DIR      Local output directory (default: ${DEFAULT_OUTPUT_DIR})
    --tables TABLES       Comma-separated list of tables (default: all)
    --dry-run             Show what would be done without executing
    --help                Show this help message

ENVIRONMENT VARIABLES:
    CRE_NODE_NAME         Erlang node name
    ERLANG_COOKIE         Erlang cookie
    GCS_BUCKET            GCS bucket name
    OUTPUT_DIR            Local output directory
    TABLES                Tables to export
    DRY_RUN               Set to "true" for dry-run mode

EXAMPLES:
    # Export all tables to default bucket
    ${SCRIPT_NAME}

    # Dry run to see what would be exported
    ${SCRIPT_NAME} --dry-run

    # Export specific tables
    ${SCRIPT_NAME} --tables case_table,workflow_table

    # Export to custom bucket with specific node
    ${SCRIPT_NAME} --node cre@prod-node --bucket my-backups

EXIT CODES:
    0    Success
    1    General error
    2    Validation error
    3    Mnesia connection error
    4    Export failed
    5    GCS upload failed

EOF
}

# =============================================================================
# ARGUMENT PARSING
# =============================================================================

parse_arguments() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --node)
                NODE_NAME="$2"
                shift 2
                ;;
            --cookie)
                ERLANG_COOKIE="$2"
                shift 2
                ;;
            --bucket)
                GCS_BUCKET="$2"
                shift 2
                ;;
            --output-dir)
                OUTPUT_DIR="$2"
                shift 2
                ;;
            --tables)
                TABLES="$2"
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
# VALIDATION FUNCTIONS
# =============================================================================

validate_arguments() {
    local errors=0

    # Validate node name format
    if [[ ! "$NODE_NAME" =~ ^[a-zA-Z0-9_-]+@.+$ ]]; then
        log_error "Invalid node name format: $NODE_NAME"
        log_error "Expected format: name@hostname"
        ((errors++))
    fi

    # Validate GCS bucket name format
    if [[ ! "$GCS_BUCKET" =~ ^gs://|[a-z0-9][a-z0-9._-]{1,60}[a-z0-9]$ ]]; then
        log_warning "GCS bucket name may not be valid: $GCS_BUCKET"
    fi

    # Validate output directory
    if [[ -e "$OUTPUT_DIR" && ! -d "$OUTPUT_DIR" ]]; then
        log_error "Output path exists but is not a directory: $OUTPUT_DIR"
        ((errors++))
    fi

    return $errors
}

# =============================================================================
# ERLANG FUNCTIONS
# =============================================================================

# Create Erlang script for Mnesia export
create_erl_export_script() {
    local tables="$1"
    local output_file="$2"

    cat > "$output_file" <<'ERL_EOF'
#!/usr/bin/env escript
%% -*- erlang -*-
-mode(compile).

main([TablesArg, OutputDir]) ->
    Tables = case TablesArg of
        "all" -> mnesia:system_info(tables);
        TablesStr -> string:split(TablesStr, ",", all)
    end,

    io:format("Exporting ~p tables to ~s~n", [length(Tables), OutputDir]),

    % Create output directory
    ok = filelib:ensure_dir(filename:join(OutputDir, "dummy")),

    % Export each table
    ExportResults = lists:map(fun(Table) ->
        export_table(Table, OutputDir)
    end, Tables),

    % Print summary
    {Success, Failed} = lists:foldl(fun
        ({ok, _}, {S, F}) -> {S + 1, F};
        ({error, _}, {S, F}) -> {S, F + 1}
    end, {0, 0}, ExportResults),

    io:format("Export complete: ~p succeeded, ~p failed~n", [Success, Failed]),

    case Failed of
        0 -> halt(0);
        _ -> halt(1)
    end.

export_table(Table, OutputDir) ->
    try
        % Get all records from table
        case mnesia:transaction(fun() -> mnesia:match_object(Table, mnesia:table_info(Table, wild_pattern), read) end) of
            {atomic, Records} ->
                % Convert records to JSON-able format
                JsonData = lists:map(fun(Record) when is_tuple(Record) ->
                    RecordList = tuple_to_list(Record),
                    case RecordList of
                        [TableName | Fields] when is_atom(TableName) ->
                            #{table => TableName, data => Fields};
                        _ ->
                            #{raw => RecordList}
                    end
                end, Records),

                % Write to JSON file
                Filename = filename:join(OutputDir, atom_to_list(Table) ++ ".json"),
                JsonString = jsx:encode(JsonData),
                ok = file:write_file(Filename, JsonString),

                io:format("  Exported ~p: ~p records~n", [Table, length(Records)]),
                {ok, {Table, length(Records)}};
            {aborted, Reason} ->
                io:format("  Failed to export ~p: ~p~n", [Table, Reason]),
                {error, {Table, Reason}}
        end
    catch
        _:Error ->
            io:format("  Error exporting ~p: ~p~n", [Table, Error]),
            {error, {Table, Error}}
    end.
ERL_EOF

    chmod +x "$output_file"
}

# Check Mnesia connectivity
check_mnesia_connectivity() {
    show_progress "Checking Mnesia connectivity"

    local erl_cmd
    erl_cmd=$(cat <<EOF
net_adm:ping('${NODE_NAME}') =:= pong
andalso begin
    case rpc:call('${NODE_NAME}', mnesia, system_info, [is_running]) of
        yes -> true;
        _ -> false
    end
end
EOF
)

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would check Mnesia connectivity to $NODE_NAME"
        return 0
    fi

    local result
    result=$(erl -noshell -name "mnesia_export_${$}@localhost" -eval "$erl_cmd" -s init stop 2>&1)

    if [[ "$result" == "true" ]]; then
        log_success "Connected to Mnesia at $NODE_NAME"
        return 0
    else
        log_error "Failed to connect to Mnesia at $NODE_NAME"
        log_error "Ensure CRE node is running and accessible"
        return 1
    fi
}

# Get list of tables to export
get_tables_list() {
    local erl_cmd
    erl_cmd=$(cat <<EOF
Tables = case mnesia:system_info(tables) of
    All -> lists:filter(fun(T) -> T =/= schema end, All)
end,
io:format("~s~n", [string:join([atom_to_list(T) || T <- Tables], ",")]).
EOF
)

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would list tables from Mnesia"
        echo "schema,case_table,workflow_table"  # Example
        return 0
    fi

    erl -noshell -name "mnesia_export_${$}@localhost" \
        -eval "net_adm:ping('${NODE_NAME}')" \
        -eval "$erl_cmd" \
        -s init stop 2>&1 | grep -vE "^$|Erlang/OTP"
}

# =============================================================================
# EXPORT FUNCTIONS
# =============================================================================

# Create export directory
setup_export_directory() {
    show_progress "Setting up export directory"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would create directory: $OUTPUT_DIR"
        return 0
    fi

    if [[ -d "$OUTPUT_DIR" ]]; then
        log_info "Using existing directory: $OUTPUT_DIR"
    else
        mkdir -p "$OUTPUT_DIR"
        log_success "Created export directory: $OUTPUT_DIR"
    fi

    # Create export metadata file
    cat > "${OUTPUT_DIR}/export_metadata.json" <<EOF
{
    "export_id": "${EXPORT_ID}",
    "timestamp": "${TIMESTAMP}",
    "node": "${NODE_NAME}",
    "tables": "${TABLES}",
    "hostname": "$(hostname -f)",
    "user": "$(whoami)"
}
EOF

    return 0
}

# Export Mnesia tables
export_mnesia_tables() {
    show_progress "Exporting Mnesia tables"

    local erl_script="${OUTPUT_DIR}/mnesia_export.erl"
    create_erl_export_script "$TABLES" "$erl_script"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would export tables: $TABLES"
        log_info "[DRY-RUN] Would write to: $OUTPUT_DIR"
        return 0
    fi

    # Run the export script
    local cookie_arg=""
    if [[ -n "$ERLANG_COOKIE" ]]; then
        cookie_arg="-setcookie \"$ERLANG_COOKIE\""
    fi

    if escript "$erl_script" "$TABLES" "$OUTPUT_DIR"; then
        log_success "Mnesia export completed"

        # Count exported records
        local total_records=0
        for json_file in "${OUTPUT_DIR}"/*.json; do
            if [[ -f "$json_file" && "$json_file" != *"export_metadata"* ]]; then
                local count
                count=$(jq 'length' "$json_file" 2>/dev/null || echo "0")
                total_records=$((total_records + count))
                log_info "  $(basename "$json_file"): $count records"
            fi
        done

        log_success "Total records exported: $total_records"
        return 0
    else
        log_error "Mnesia export failed"
        return 1
    fi
}

# Create export checksums
create_checksums() {
    show_progress "Creating export checksums"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would create checksums"
        return 0
    fi

    local checksum_file="${OUTPUT_DIR}/SHA256SUMS"

    # Create SHA256 checksums for all JSON files
    (cd "$OUTPUT_DIR" && sha256sum *.json > "$checksum_file" 2>/dev/null || true)

    if [[ -f "$checksum_file" ]]; then
        log_success "Checksums created: $checksum_file"
        return 0
    else
        log_warning "Failed to create checksums"
        return 1
    fi
}

# =============================================================================
# GCS FUNCTIONS
# =============================================================================

# Verify GCS bucket access
verify_gcs_bucket() {
    show_progress "Verifying GCS bucket access"

    local bucket_url="gs://${GCS_BUCKET}"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would verify GCS bucket: $bucket_url"
        return 0
    fi

    # Check if gsutil is available (via gcloud)
    if ! command_exists gsutil; then
        log_error "gsutil not found. Please install Google Cloud SDK."
        return 1
    fi

    # Check if bucket exists and is accessible
    if gsutil -q ls "$bucket_url" 2>/dev/null; then
        log_success "GCS bucket accessible: $bucket_url"
        return 0
    else
        log_warning "GCS bucket not found or not accessible: $bucket_url"
        log_info "Attempting to create bucket..."
        if gsutil mb -p "$(gcloud config get-value project)" "$bucket_url" 2>/dev/null; then
            log_success "Created GCS bucket: $bucket_url"
            return 0
        else
            log_error "Failed to access or create bucket: $bucket_url"
            return 1
        fi
    fi
}

# Upload export to GCS
upload_to_gcs() {
    show_progress "Uploading export to GCS"

    local gcs_path="gs://${GCS_BUCKET}/mnesia-exports/${EXPORT_ID}"
    local local_path="${OUTPUT_DIR}/*"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would upload files to: $gcs_path"
        log_info "[DRY-RUN] From: $OUTPUT_DIR"
        return 0
    fi

    log_info "Uploading to: $gcs_path"

    # Use gsutil to upload with parallel processing
    if gsutil -m cp -r "$OUTPUT_DIR" "$gcs_path"; then
        log_success "Upload completed successfully"

        # Set object metadata
        gsutil setmeta -h "Content-Type:application/json" \
            "${gcs_path}/*.json" 2>/dev/null || true

        # Make files publicly readable if desired (commented for security)
        # gsutil acl ch -u AllUsers:R "${gcs_path}/*.json" 2>/dev/null || true

        return 0
    else
        log_error "Upload to GCS failed"
        return 1
    fi
}

# =============================================================================
# CLEANUP FUNCTIONS
# =============================================================================

cleanup_local_files() {
    show_progress "Cleaning up local files"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY-RUN] Would keep local files: $OUTPUT_DIR"
        return 0
    fi

    # Optionally remove local export files after successful upload
    # Uncomment the following lines to enable cleanup:
    # if [[ -d "$OUTPUT_DIR" ]]; then
    #     rm -rf "$OUTPUT_DIR"
    #     log_success "Removed local export directory"
    # fi

    log_info "Local files retained at: $OUTPUT_DIR"
    return 0
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

main() {
    local start_time end_time duration

    # Record start time
    start_time=$(date +%s)

    log_info "Starting Mnesia export: ${SCRIPT_NAME} v${SCRIPT_VERSION}"
    log_info "Export ID: ${EXPORT_ID}"
    echo

    # Parse command-line arguments
    parse_arguments "$@"

    # Validate requirements
    if ! validate_requirements; then
        exit 2
    fi

    # Validate arguments
    if ! validate_arguments; then
        exit 2
    fi

    # Show configuration
    log_info "Configuration:"
    echo "  Node:         $NODE_NAME"
    echo "  Bucket:       gs://${GCS_BUCKET}"
    echo "  Output:       $OUTPUT_DIR"
    echo "  Tables:       $TABLES"
    echo "  Dry Run:      $DRY_RUN"
    echo

    # Execute export pipeline
    if ! setup_export_directory; then
        log_error "Failed to setup export directory"
        exit 4
    fi

    if ! check_mnesia_connectivity; then
        log_error "Mnesia connectivity check failed"
        exit 3
    fi

    if ! export_mnesia_tables; then
        log_error "Mnesia export failed"
        exit 4
    fi

    if ! create_checksums; then
        log_warning "Checksum creation failed (continuing)"
    fi

    if ! verify_gcs_bucket; then
        log_error "GCS bucket verification failed"
        exit 5
    fi

    if ! upload_to_gcs; then
        log_error "GCS upload failed"
        exit 5
    fi

    cleanup_local_files

    # Calculate duration
    end_time=$(date +%s)
    duration=$((end_time - start_time))

    echo
    log_success "Mnesia export completed successfully!"
    log_info "Export location: gs://${GCS_BUCKET}/mnesia-exports/${EXPORT_ID}"
    log_info "Duration: ${duration}s"
    log_info "Export ID: ${EXPORT_ID}"

    return 0
}

# Trap errors
trap 'log_error "Script failed at line $LINENO"' ERR

# Run main function
main "$@"
