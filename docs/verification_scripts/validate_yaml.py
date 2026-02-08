#!/usr/bin/env python3
"""
validate_yaml.py - Check YAML 0.2 conformance for YAWL workflow specifications

This script validates YAML workflow specifications against the YAWL YAML 0.2
format defined in wf_yaml_spec.erl. It checks for required fields, proper
data types, and structural correctness.

Usage:
    python validate_yaml.py path/to/workflow.yaml
    python validate_yaml.py --json path/to/workflow.yaml
    python validate_yaml.py --directory src/yaws/

Output formats:
    - Default: Human-readable text with TAP-style output
    --json: Structured JSON for automation/CI

Exit codes:
    0: All checks passed
    1: Validation errors found
    2: File access errors
"""

import argparse
import json
import re
import sys
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List, Optional, Set, Tuple, Union


class CheckStatus(Enum):
    """Status of a validation check."""
    PASS = "PASS"
    FAIL = "FAIL"
    WARN = "WARN"
    SKIP = "SKIP"


@dataclass
class ValidationResult:
    """Result of a single validation check."""
    check_id: str
    description: str
    status: CheckStatus
    details: str = ""
    path: str = ""  # JSON path to the invalid element


@dataclass
class YAMLValidationReport:
    """Complete validation report for a YAML file."""
    file_path: str
    yaml_version: Optional[str] = None
    schema_version: Optional[str] = None
    workflow_id: Optional[str] = None
    results: List[ValidationResult] = field(default_factory=list)
    patterns_found: Set[str] = field(default_factory=set)

    def add_result(self, check_id: str, description: str,
                   status: CheckStatus, details: str = "", path: str = ""):
        """Add a validation result to the report."""
        self.results.append(ValidationResult(
            check_id=check_id,
            description=description,
            status=status,
            details=details,
            path=path
        ))

    def to_dict(self) -> Dict[str, Any]:
        """Convert report to dictionary for JSON serialization."""
        passed = sum(1 for r in self.results if r.status == CheckStatus.PASS)
        failed = sum(1 for r in self.results if r.status == CheckStatus.FAIL)
        warnings = sum(1 for r in self.results if r.status == CheckStatus.WARN)

        return {
            "file": self.file_path,
            "yaml_version": self.yaml_version,
            "schema_version": self.schema_version,
            "workflow_id": self.workflow_id,
            "summary": {
                "total": len(self.results),
                "passed": passed,
                "failed": failed,
                "warnings": warnings,
                "overall": "PASS" if failed == 0 else "FAIL"
            },
            "patterns_found": sorted(self.patterns_found),
            "results": [
                {
                    "check_id": r.check_id,
                    "description": r.description,
                    "status": r.status.value,
                    "details": r.details,
                    "path": r.path
                }
                for r in self.results
            ]
        }


class YAMLValidator:
    """Validator for YAWL YAML 0.2 format specifications."""

    # Valid YAML versions
    VALID_YAML_VERSIONS = {"0.2"}

    # Valid schema versions
    VALID_SCHEMA_VERSIONS = {"2.1"}

    # Required top-level keys
    REQUIRED_TOP_LEVEL_KEYS = {
        "yawl_yaml_version",
        "specificationSet"
    }

    # Required keys in specificationSet
    REQUIRED_SPEC_KEYS = {
        "yawl_schema_version",
        "uri",
        "rootNet",
        "nets"
    }

    # All 43 YAWL pattern IDs
    ALL_PATTERNS = {
        "P1_Sequence", "P2_ParallelSplit", "P3_Synchronization",
        "P4_ExclusiveChoice", "P5_SimpleMerge", "P6_MultipleChoice",
        "P7_StructuredSyncMerge", "P8_MultipleMerge", "P9_Discriminator",
        "P10_ArbitraryCycles", "P11_ImplicitTermination", "P12_MI_NoSync",
        "P13_MI_DesignTime", "P14_MI_RuntimeKnown", "P15_MI_RuntimeUnknown",
        "P16_DeferredChoice", "P17_InterleavedParallelRouting", "P18_Milestone",
        "P19_CancelActivity", "P20_CancelCase", "P21_StructuredLoop",
        "P22_Recursion", "P23_TransientTrigger", "P24_PersistentTrigger",
        "P25_CancelRegion", "P26_CancelMIActivity", "P27_CompleteMIActivity",
        "P28_BlockingDiscriminator", "P29_CancellingDiscriminator",
        "P30_StructuredPartialJoin", "P31_BlockingPartialJoin",
        "P32_CancellingPartialJoin", "P33_GeneralizedANDJoin",
        "P34_StaticPartialJoinMI", "P35_CancellingPartialJoinMI",
        "P36_DynamicPartialJoinMI", "P37_LocalSyncMerge", "P38_GeneralSyncMerge",
        "P39_CriticalSection", "P40_InterleavedRouting", "P41_ThreadMerge",
        "P42_ThreadSplit", "P43_ExplicitTermination"
    }

    # Valid node kinds
    VALID_NODE_KINDS = {
        "task", "condition", "inputCondition", "outputCondition"
    }

    # Valid task types
    VALID_TASK_TYPES = {
        "human", "automated", "service"
    }

    def __init__(self, data: Dict[str, Any], file_path: str):
        """Initialize validator with parsed YAML data.

        Args:
            data: Parsed YAML dictionary
            file_path: Path to the YAML file (for error reporting)
        """
        self.data = data
        self.file_path = file_path
        self.report = YAMLValidationReport(file_path=file_path)

    def validate(self) -> YAMLValidationReport:
        """Run all validation checks.

        Returns:
            Complete validation report
        """
        # Validate top-level structure
        self._validate_top_level()

        # Validate specificationSet
        if "specificationSet" in self.data:
            self._validate_specification_set(self.data["specificationSet"])
        else:
            # Already reported as missing
            pass

        return self.report

    def _get_value(self, data: Dict, path: List[str], default=None):
        """Get a value from nested dict using path list."""
        current = data
        for key in path:
            if isinstance(current, dict) and key in current:
                current = current[key]
            else:
                return default
        return current

    def _validate_top_level(self):
        """Validate top-level YAML structure."""
        # Check yawl_yaml_version
        if "yawl_yaml_version" in self.data:
            version = str(self.data["yawl_yaml_version"])
            self.report.yaml_version = version
            if version in self.VALID_YAML_VERSIONS:
                self.report.add_result(
                    "VER_001",
                    "YAML version valid",
                    CheckStatus.PASS,
                    f"Found version {version}"
                )
            else:
                self.report.add_result(
                    "VER_001",
                    "YAML version valid",
                    CheckStatus.WARN,
                    f"Version {version} not in supported versions: {self.VALID_YAML_VERSIONS}",
                    "/yawl_yaml_version"
                )
        else:
            self.report.add_result(
                "VER_001",
                "YAML version valid",
                CheckStatus.FAIL,
                "Missing required field: yawl_yaml_version",
                "/yawl_yaml_version"
            )

        # Check specificationSet exists
        if "specificationSet" in self.data:
            if isinstance(self.data["specificationSet"], dict):
                self.report.add_result(
                    "TOP_001",
                    "specificationSet present and valid",
                    CheckStatus.PASS
                )
            else:
                self.report.add_result(
                    "TOP_001",
                    "specificationSet present and valid",
                    CheckStatus.FAIL,
                    "specificationSet must be a dictionary/map",
                    "/specificationSet"
                )
        else:
            self.report.add_result(
                "TOP_001",
                "specificationSet present and valid",
                CheckStatus.FAIL,
                "Missing required key: specificationSet",
                "/specificationSet"
            )

        # Check for unexpected top-level keys (warn)
        valid_keys = self.REQUIRED_TOP_LEVEL_KEYS | {"specificationSet"}
        for key in self.data.keys():
            if key not in valid_keys and not key.startswith("x_"):
                self.report.add_result(
                    "TOP_002",
                    f"No unexpected top-level keys",
                    CheckStatus.WARN,
                    f"Unexpected key: {key}",
                    f"/{key}"
                )

    def _validate_specification_set(self, spec_set: Dict[str, Any]):
        """Validate the specificationSet section."""
        base_path = "/specificationSet"

        # Check schema version
        if "yawl_schema_version" in spec_set:
            version = str(spec_set["yawl_schema_version"])
            self.report.schema_version = version
            if version in self.VALID_SCHEMA_VERSIONS:
                self.report.add_result(
                    "SCH_001",
                    "Schema version valid",
                    CheckStatus.PASS,
                    f"Found version {version}",
                    f"{base_path}/yawl_schema_version"
                )
            else:
                self.report.add_result(
                    "SCH_001",
                    "Schema version valid",
                    CheckStatus.WARN,
                    f"Version {version} not in recommended versions: {self.VALID_SCHEMA_VERSIONS}",
                    f"{base_path}/yawl_schema_version"
                )
        else:
            self.report.add_result(
                "SCH_001",
                "Schema version valid",
                CheckStatus.WARN,
                "Missing recommended field: yawl_schema_version",
                f"{base_path}/yawl_schema_version"
            )

        # Check URI
        if "uri" in spec_set:
            uri = spec_set["uri"]
            if uri and isinstance(uri, str):
                self.report.workflow_id = uri
                self.report.add_result(
                    "ID_001",
                    "Workflow URI present",
                    CheckStatus.PASS,
                    f"Found: {uri}",
                    f"{base_path}/uri"
                )
            else:
                self.report.add_result(
                    "ID_001",
                    "Workflow URI present",
                    CheckStatus.WARN,
                    "URI field exists but is not a non-empty string",
                    f"{base_path}/uri"
                )
        else:
            self.report.add_result(
                "ID_001",
                "Workflow URI present",
                CheckStatus.WARN,
                "Missing recommended field: uri",
                f"{base_path}/uri"
            )

        # Check rootNet
        if "rootNet" in spec_set:
            root_net = spec_set["rootNet"]
            if isinstance(root_net, str) and root_net:
                self.report.add_result(
                    "NET_001",
                    "rootNet specified",
                    CheckStatus.PASS,
                    f"Root net: {root_net}",
                    f"{base_path}/rootNet"
                )
            else:
                self.report.add_result(
                    "NET_001",
                    "rootNet specified",
                    CheckStatus.FAIL,
                    "rootNet must be a non-empty string",
                    f"{base_path}/rootNet"
                )
        else:
            self.report.add_result(
                "NET_001",
                "rootNet specified",
                CheckStatus.WARN,
                "Missing recommended field: rootNet",
                f"{base_path}/rootNet"
            )

        # Validate nets
        if "nets" in spec_set:
            nets = spec_set["nets"]
            if isinstance(nets, list):
                self.report.add_result(
                    "NET_002",
                    "nets array valid",
                    CheckStatus.PASS,
                    f"Found {len(nets)} net(s)"
                )
                self._validate_nets(nets, base_path)
            else:
                self.report.add_result(
                    "NET_002",
                    "nets array valid",
                    CheckStatus.FAIL,
                    "nets must be an array",
                    f"{base_path}/nets"
                )
        else:
            self.report.add_result(
                "NET_002",
                "nets array valid",
                CheckStatus.FAIL,
                "Missing required field: nets",
                f"{base_path}/nets"
            )

        # Validate roles (optional)
        if "roles" in spec_set:
            roles = spec_set["roles"]
            if isinstance(roles, list):
                self.report.add_result(
                    "ROL_001",
                    "roles array valid",
                    CheckStatus.PASS,
                    f"Found {len(roles)} role(s)"
                )
            else:
                self.report.add_result(
                    "ROL_001",
                    "roles array valid",
                    CheckStatus.WARN,
                    "roles must be an array if present",
                    f"{base_path}/roles"
                )

        # Validate pattern_instances (optional)
        if "pattern_instances" in spec_set:
            patterns = spec_set["pattern_instances"]
            if isinstance(patterns, list):
                self._validate_pattern_instances(patterns, base_path)
            else:
                self.report.add_result(
                    "PAT_001",
                    "pattern_instances array valid",
                    CheckStatus.WARN,
                    "pattern_instances must be an array",
                    f"{base_path}/pattern_instances"
                )

    def _validate_nets(self, nets: List[Dict], base_path: str):
        """Validate net definitions."""
        for i, net in enumerate(nets):
            if not isinstance(net, dict):
                self.report.add_result(
                    f"NET_{i+3:03d}",
                    f"Net {i} is a dictionary",
                    CheckStatus.FAIL,
                    f"Net at index {i} is not a dictionary",
                    f"{base_path}/nets/{i}"
                )
                continue

            net_path = f"{base_path}/nets/{i}"

            # Check net id
            if "id" in net:
                net_id = net["id"]
                if isinstance(net_id, str) and net_id:
                    self.report.add_result(
                        f"NET_{i+3:03d}",
                        f"Net {i} has valid ID",
                        CheckStatus.PASS,
                        f"Net ID: {net_id}",
                        f"{net_path}/id"
                    )
                else:
                    self.report.add_result(
                        f"NET_{i+3:03d}",
                        f"Net {i} has valid ID",
                        CheckStatus.WARN,
                        "Net ID should be a non-empty string",
                        f"{net_path}/id"
                    )
            else:
                self.report.add_result(
                    f"NET_{i+3:03d}",
                    f"Net {i} has valid ID",
                    CheckStatus.WARN,
                    "Net missing 'id' field",
                    f"{net_path}/id"
                )

            # Validate nodes
            if "nodes" in net:
                nodes = net["nodes"]
                if isinstance(nodes, list):
                    self._validate_nodes(nodes, net_path)
                else:
                    self.report.add_result(
                        f"NET_{i+3:03d}",
                        f"Net {i} nodes array valid",
                        CheckStatus.WARN,
                        "nodes must be an array",
                        f"{net_path}/nodes"
                    )

            # Validate flows
            if "flows" in net:
                flows = net["flows"]
                if isinstance(flows, list):
                    self._validate_flows(flows, net_path)
                else:
                    self.report.add_result(
                        f"NET_{i+3:03d}",
                        f"Net {i} flows array valid",
                        CheckStatus.WARN,
                        "flows must be an array",
                        f"{net_path}/flows"
                    )

    def _validate_nodes(self, nodes: List[Dict], net_path: str):
        """Validate node definitions within a net."""
        for i, node in enumerate(nodes):
            if not isinstance(node, dict):
                continue

            node_path = f"{net_path}/nodes/{i}"

            # Check node kind
            if "kind" in node:
                kind = node["kind"]
                if kind in self.VALID_NODE_KINDS:
                    self.report.add_result(
                        f"NODE_{i+1:03d}",
                        f"Node {i} has valid kind",
                        CheckStatus.PASS if i == 0 else CheckStatus.SKIP,
                        f"Kind: {kind}",
                        f"{node_path}/kind"
                    )
                else:
                    self.report.add_result(
                        f"NODE_{i+1:03d}",
                        f"Node {i} has valid kind",
                        CheckStatus.WARN,
                        f"Unknown kind: {kind}. Expected one of: {self.VALID_NODE_KINDS}",
                        f"{node_path}/kind"
                    )
            else:
                self.report.add_result(
                    f"NODE_{i+1:03d}",
                    f"Node {i} has kind field",
                    CheckStatus.WARN,
                    "Node missing 'kind' field",
                    f"{node_path}/kind"
                )

            # Check node id
            if "id" in node:
                node_id = node["id"]
                if isinstance(node_id, str) and node_id:
                    pass  # ID valid
                else:
                    self.report.add_result(
                        f"NODE_{i+1:03d}",
                        f"Node {i} has valid ID",
                        CheckStatus.WARN,
                        "Node ID should be a non-empty string",
                        f"{node_path}/id"
                    )

    def _validate_flows(self, flows: List[Dict], net_path: str):
        """Validate flow definitions within a net."""
        for i, flow in enumerate(flows):
            if not isinstance(flow, dict):
                continue

            flow_path = f"{net_path}/flows/{i}"

            # Check required from/to fields
            has_from = "from" in flow
            has_to = "to" in flow

            if has_from and has_to:
                # Just check first flow
                if i == 0:
                    self.report.add_result(
                        f"FLOW_{i+1:03d}",
                        f"Flow {i} has required fields",
                        CheckStatus.PASS,
                        f"From: {flow['from']} -> To: {flow['to']}",
                        flow_path
                    )
            else:
                missing = []
                if not has_from:
                    missing.append("from")
                if not has_to:
                    missing.append("to")
                self.report.add_result(
                    f"FLOW_{i+1:03d}",
                    f"Flow {i} has required fields",
                    CheckStatus.WARN,
                    f"Missing: {', '.join(missing)}",
                    flow_path
                )

    def _validate_pattern_instances(self, patterns: List[Dict], base_path: str):
        """Validate pattern instance definitions."""
        for i, pattern in enumerate(patterns):
            if not isinstance(pattern, dict):
                continue

            pattern_path = f"{base_path}/pattern_instances/{i}"

            # Check pattern field
            if "pattern" in pattern:
                pattern_name = pattern["pattern"]
                if isinstance(pattern_name, str):
                    if pattern_name in self.ALL_PATTERNS:
                        self.report.patterns_found.add(pattern_name)
                        if len(self.report.patterns_found) <= 5:  # Limit reports
                            self.report.add_result(
                                f"PAT_{i+1:03d}",
                                f"Pattern {i} uses valid pattern ID",
                                CheckStatus.PASS,
                                f"Pattern: {pattern_name}",
                                pattern_path
                            )
                    else:
                        self.report.add_result(
                            f"PAT_{i+1:03d}",
                            f"Pattern {i} uses valid pattern ID",
                            CheckStatus.WARN,
                            f"Unknown pattern: {pattern_name}. Expected P1-P43 pattern",
                            f"{pattern_path}/pattern"
                        )
                else:
                    self.report.add_result(
                        f"PAT_{i+1:03d}",
                        f"Pattern {i} uses valid pattern ID",
                        CheckStatus.WARN,
                        "pattern field must be a string",
                        f"{pattern_path}/pattern"
                    )
            else:
                self.report.add_result(
                    f"PAT_{i+1:03d}",
                    f"Pattern {i} has pattern field",
                    CheckStatus.WARN,
                    "Pattern instance missing 'pattern' field",
                    pattern_path
                )


def parse_yaml_file(file_path: str) -> Optional[Dict[str, Any]]:
    """Parse a YAML file without external dependencies.

    Returns:
        Parsed YAML data or None if parsing fails
    """
    try:
        # Try to use yaml module if available
        import yaml
        with open(file_path, 'r', encoding='utf-8') as f:
            return yaml.safe_load(f)
    except ImportError:
        # Fallback: simple YAML parser for basic structure
        return _simple_yaml_parse(file_path)
    except Exception as e:
        print(f"Error parsing YAML: {e}", file=sys.stderr)
        return None


def _simple_yaml_parse(file_path: str) -> Optional[Dict[str, Any]]:
    """Very basic YAML parser for simple key-value and list structures.

    This is a fallback when PyYAML is not available. It handles
    basic YAML structure but may fail on complex files.
    """
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            lines = f.readlines()

        result = {}
        stack = [result]
        list_stack = []
        indent_level = 0

        for line in lines:
            stripped = line.lstrip()
            if not stripped or stripped.startswith('#'):
                continue

            indent = len(line) - len(stripped)
            current = stack[-1] if stack else result

            # Handle key: value
            if ':' in stripped:
                key, value = stripped.split(':', 1)
                key = key.strip()
                value = value.strip()

                if value == '':
                    # Start of new mapping or sequence
                    current[key] = {}
                    stack.append(current[key])
                elif value.startswith('[') and value.endswith(']'):
                    # Inline array
                    current[key] = [item.strip() for item in value[1:-1].split(',')]
                else:
                    # Simple value
                    current[key] = _parse_yaml_value(value)
            # Handle list items
            elif stripped.startswith('- '):
                value = stripped[2:].strip()
                if isinstance(current, list):
                    current.append(_parse_yaml_value(value))

        return result
    except Exception:
        return None


def _parse_yaml_value(value: str) -> Any:
    """Parse a YAML scalar value."""
    value = value.strip()

    # Boolean
    if value.lower() == 'true':
        return True
    if value.lower() == 'false':
        return False

    # Null
    if value.lower() in ('null', '~', ''):
        return None

    # Number
    try:
        if '.' in value:
            return float(value)
        return int(value)
    except ValueError:
        pass

    # Quoted string
    if (value.startswith('"') and value.endswith('"')) or \
       (value.startswith("'") and value.endswith("'")):
        return value[1:-1]

    # Default: return as string
    return value


def print_tap_report(report: YAMLValidationReport, check_number: int) -> int:
    """Print validation report in TAP (Test Anything Protocol) format.

    Returns:
        Next check number
    """
    print(f"{'ok' if all(r.status != CheckStatus.FAIL for r in report.results) else 'not ok'} "
          f"{check_number} - {report.file_path}")

    for result in report.results:
        if result.status == CheckStatus.FAIL:
            print(f"  # Failed: {result.check_id} - {result.description}")
            if result.details:
                print(f"  #   {result.details}")
            if result.path:
                print(f"  #   Path: {result.path}")
        elif result.status == CheckStatus.WARN:
            print(f"  # Warning: {result.check_id} - {result.description}")
            if result.details:
                print(f"  #   {result.details}")

    return check_number + 1


def print_text_report(report: YAMLValidationReport, verbose: bool = False) -> int:
    """Print validation report in human-readable text format.

    Returns:
        Exit code (0 for pass, 1 for failure)
    """
    print(f"\n{'=' * 70}")
    print(f"YAML Validation Report: {report.file_path}")
    print(f"{'=' * 70}\n")

    if report.yaml_version:
        print(f"YAML Version: {report.yaml_version}")
    if report.schema_version:
        print(f"Schema Version: {report.schema_version}")
    if report.workflow_id:
        print(f"Workflow ID: {report.workflow_id}")
    if report.patterns_found:
        print(f"Patterns: {', '.join(sorted(report.patterns_found))}")
    print()

    # Summary
    passed = sum(1 for r in report.results if r.status == CheckStatus.PASS)
    failed = sum(1 for r in report.results if r.status == CheckStatus.FAIL)
    warnings = sum(1 for r in report.results if r.status == CheckStatus.WARN)

    print(f"Summary: {passed} passed, {warnings} warnings, {failed} failed\n")

    # Failed checks
    if failed > 0:
        print("FAILED CHECKS:")
        for result in report.results:
            if result.status == CheckStatus.FAIL:
                path = f" [{result.path}]" if result.path else ""
                print(f"  [FAIL] {result.check_id}: {result.description}{path}")
                if result.details:
                    print(f"        {result.details}")
        print()

    # Warnings
    if warnings > 0:
        print("WARNINGS:")
        for result in report.results:
            if result.status == CheckStatus.WARN:
                path = f" [{result.path}]" if result.path else ""
                print(f"  [WARN] {result.check_id}: {result.description}{path}")
                if result.details:
                    print(f"        {result.details}")
        print()

    # Verbose passing checks
    if verbose and passed > 0:
        print("PASSED CHECKS:")
        for result in report.results:
            if result.status == CheckStatus.PASS:
                path = f" [{result.path}]" if result.path else ""
                print(f"  [PASS] {result.check_id}: {result.description}{path}")
        print()

    return 0 if failed == 0 else 1


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Validate YAWL YAML 0.2 workflow specifications"
    )
    parser.add_argument(
        "path",
        help="Path to YAML file or directory containing YAML files"
    )
    parser.add_argument(
        "--json", "-j",
        action="store_true",
        help="Output results in JSON format"
    )
    parser.add_argument(
        "--tap",
        action="store_true",
        help="Output in TAP (Test Anything Protocol) format"
    )
    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Show detailed passing checks"
    )
    parser.add_argument(
        "--output", "-o",
        help="Write output to file instead of stdout"
    )

    args = parser.parse_args()

    # Resolve path
    path = Path(args.path)
    if not path.exists():
        print(f"Error: Path not found: {args.path}", file=sys.stderr)
        return 2

    # Find YAML files
    if path.is_dir():
        yaml_files = list(path.rglob("*.yaml")) + list(path.rglob("*.yml"))
    else:
        yaml_files = [path]

    if not yaml_files:
        print("Error: No YAML files found", file=sys.stderr)
        return 2

    all_reports = []
    exit_code = 0

    for yaml_file in yaml_files:
        data = parse_yaml_file(str(yaml_file))
        if data is None:
            print(f"Error: Could not parse {yaml_file}", file=sys.stderr)
            exit_code = 2
            continue

        if not isinstance(data, dict):
            print(f"Error: YAML root must be a dictionary: {yaml_file}", file=sys.stderr)
            exit_code = 2
            continue

        validator = YAMLValidator(data, str(yaml_file))
        report = validator.validate()
        all_reports.append(report)

        if any(r.status == CheckStatus.FAIL for r in report.results):
            exit_code = 1

    # Generate output
    if args.tap:
        print(f"TAP version 13")
        print(f"1..{len(all_reports)}")
        check_num = 1
        for report in all_reports:
            check_num = print_tap_report(report, check_num)
    elif args.json:
        if len(all_reports) == 1:
            output = json.dumps(all_reports[0].to_dict(), indent=2)
        else:
            output = json.dumps(
                {"reports": [r.to_dict() for r in all_reports]},
                indent=2
            )
        print(output)
    else:
        if len(all_reports) == 1:
            exit_code = print_text_report(all_reports[0], args.verbose)
        else:
            # Summary for multiple files
            print(f"\nYAML Validation: {len(all_reports)} files scanned\n")
            for report in all_reports:
                failed = sum(1 for r in report.results if r.status == CheckStatus.FAIL)
                warnings = sum(1 for r in report.results if r.status == CheckStatus.WARN)
                status = "PASS" if failed == 0 else "FAIL"
                print(f"  [{status}] {report.file_path}: "
                      f"{failed} failed, {warnings} warnings")
            print()

    # Write to file if requested
    if args.output:
        if args.tap:
            # Capture TAP output
            from io import StringIO
            old_stdout = sys.stdout
            sys.stdout = StringIO()
            print(f"TAP version 13")
            print(f"1..{len(all_reports)}")
            check_num = 1
            for report in all_reports:
                check_num = print_tap_report(report, check_num)
            output = sys.stdout.getvalue()
            sys.stdout = old_stdout
        elif args.json:
            if len(all_reports) == 1:
                output = json.dumps(all_reports[0].to_dict(), indent=2)
            else:
                output = json.dumps(
                    {"reports": [r.to_dict() for r in all_reports]},
                    indent=2
                )
        else:
            # Text output
            from io import StringIO
            old_stdout = sys.stdout
            sys.stdout = StringIO()
            for report in all_reports:
                print_text_report(report, args.verbose)
            output = sys.stdout.getvalue()
            sys.stdout = old_stdout

        with open(args.output, 'w', encoding='utf-8') as f:
            f.write(output)
        print(f"\nOutput written to: {args.output}")

    return exit_code


if __name__ == "__main__":
    sys.exit(main())
