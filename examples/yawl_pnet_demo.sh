#!/bin/bash
#
# CRE: common runtime environment for distributed programming languages
#
# Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# -------------------------------------------------------------------
# @doc YAWL Petri Net Demo Runner
#
# This script compiles and runs the YAWL Petri Net integration example.
# It demonstrates the complete workflow of:
#   1. Starting a gen_pnet workflow net
#   2. Submitting orders for processing
#   3. Injecting external approval decisions
#   4. Observing state transitions via receipts
#
# @end
# -------------------------------------------------------------------

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CRE_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}YAWL Petri Net Integration Demo${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Ensure rebar3 is available
if ! command -v rebar3 &> /dev/null; then
    echo -e "${RED}Error: rebar3 not found in PATH${NC}"
    echo "Please install rebar3: https://rebar3.org/"
    exit 1
fi

# Step 1: Compile the project
echo -e "${YELLOW}Step 1: Compiling the project...${NC}"
cd "${CRE_ROOT}"
rebar3 compile
echo -e "${GREEN}Compilation complete${NC}"
echo ""

# Step 2: Compile the example module specifically
echo -e "${YELLOW}Step 2: Compiling example module...${NC}"
rebar3 shell --eval '
    case compile:file("'${SCRIPT_DIR}'/yawl_pnet_example.erl", [
        {i, "'${CRE_ROOT}'/include"},
        {outdir, "'${CRE_ROOT}'/ebin"},
        debug_info
    ]) of
        {ok, Module} ->
            io:format("~s: Successfully compiled~n", [Module]),
            init:stop();
        {error, Errors, Warnings} ->
            io:format("Errors: ~p~nWarnings: ~p~n", [Errors, Warnings]),
            init:stop(1)
    end.
'
echo -e "${GREEN}Example module compiled${NC}"
echo ""

# Step 3: Run the demo
echo -e "${YELLOW}Step 3: Running workflow demo...${NC}"
echo ""
echo -e "${BLUE}----------------------------------------${NC}"
echo -e "${BLUE}Demo: Order Processing with Approval${NC}"
echo -e "${BLUE}----------------------------------------${NC}"
echo ""

# Run the Erlang shell with the demo
rebar3 shell --noshell --eval "
    ${SCRIPT_DIR}/yawl_pnet_demo.erl
"

echo ""
echo -e "${GREEN}Demo completed!${NC}"
echo ""
echo -e "${BLUE}Summary:${NC}"
echo "  - Started a gen_pnet workflow process"
echo "  - Demonstrated pnet_types type definitions"
echo "  - Demonstrated pnet_marking state operations"
echo "  - Demonstrated wf_task token injection"
echo "  - Demonstrated pnet_receipt audit trail"
echo ""
echo -e "${YELLOW}To experiment interactively, run:${NC}"
echo "  cd ${CRE_ROOT}"
echo "  rebar3 shell"
echo ""
echo -e "${YELLOW}Then in the Erlang shell:${NC}"
echo "  {ok, Pid} = yawl_pnet_example:start_link()."
echo "  yawl_pnet_example:submit_order(Pid, <<\"customer1\">>, ["
echo "    #{<<\"sku\">> => <<\"PROD_BASIC\">>, <<\"quantity\">> => 2},"
echo "    #{<<\"sku\">> => <<\"PROD_STANDARD\">>, <<\"quantity\">> => 1}"
echo "  ])."
echo "  %% For high-value orders requiring approval:"
echo "  yawl_pnet_example:submit_order(Pid, <<\"customer2\">>, ["
echo "    #{<<\"sku\">> => <<\"PROD_ENTERPRISE\">>, <<\"quantity\">> => 1}"
echo "  ])."
echo "  %% Check state:"
echo "  yawl_pnet_example:get_marking(Pid)."
echo "  %% Inject approval:"
echo "  yawl_pnet_example:inject_approval(Pid, OrderId, approved)."
echo ""
