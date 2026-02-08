#!/usr/bin/env python3
"""
check_pattern_coverage.py - Verify all 43 YAWL patterns are used

This script scans Erlang source files to verify that all 43 YAWL workflow
control patterns from the van der Aalst taxonomy are implemented and used.
It checks both pattern definitions and their usage.

Usage:
    python check_pattern_coverage.py src/
    python check_pattern_coverage.py --json src/
    python check_pattern_coverage.py --pattern-module src/core/yawl_pattern_registry.erl

The 43 YAWL patterns:
  P1-P11: Basic Control Flow
  P12-P15: Multiple Instance Patterns
  P16-P18: State-based Patterns
  P19-P20: Cancellation Patterns
  P21-P22: Advanced Control Flow
  P23-P24: Trigger Patterns
  P25: Cancellation Region
  P26-P27: Multiple Instance Cancellation
  P28-P33: Advanced Synchronization
  P34-P36: Multiple Instance Partial Join
  P37-P38: Advanced Merge Patterns
  P39-P40: Concurrency Patterns
  P41-P42: Thread Patterns
  P43: Termination

Exit codes:
    0: All patterns present
    1: Missing patterns found
    2: File access errors
"""

import argparse
import json
import re
import sys
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Dict, List, Set, Tuple


class CheckStatus(Enum):
    """Status of a pattern check."""
    PRESENT = "PRESENT"
    MISSING = "MISSING"
    REFERENCED = "REFERENCED"


# All 43 YAWL patterns with their descriptions
YAWL_PATTERNS = {
    "P1": ("Sequence", "Basic Control Flow", "Sequential execution of tasks"),
    "P2": ("Parallel Split", "Basic Control Flow", "Split into parallel concurrent paths"),
    "P3": ("Synchronization", "Basic Control Flow", "Wait for all parallel paths to complete"),
    "P4": ("Exclusive Choice", "Basic Control Flow", "Choose exactly one branch from multiple options"),
    "P5": ("Simple Merge", "Basic Control Flow", "Merge multiple branches without synchronization"),
    "P6": ("Multiple Choice", "Advanced Control Flow", "Choose one or more branches from multiple options"),
    "P7": ("Structured Synchronizing Merge", "Advanced Control Flow", "Synchronize only on activated branches"),
    "P8": ("Multiple Merge", "Advanced Control Flow", "Merge multiple branches without waiting"),
    "P9": ("Discriminator", "Advanced Control Flow", "Wait for first incoming branch, disable others"),
    "P10": ("Arbitrary Cycles", "Advanced Control Flow", "Allow loops in workflow"),
    "P11": ("Implicit Termination", "Termination", "Workflow ends when no work remains"),
    "P12": ("MI without Synchronization", "Multiple Instance", "Create multiple instances, don't wait for completion"),
    "P13": ("MI design-time", "Multiple Instance", "Fixed number of instances known at design time"),
    "P14": ("MI runtime-known", "Multiple Instance", "Number of instances known at runtime"),
    "P15": ("MI runtime-unknown", "Multiple Instance", "Number of instances unknown until completion"),
    "P16": ("Deferred Choice", "State-based", "External trigger selects from pending options"),
    "P17": ("Interleaved Parallel Routing", "State-based", "Execute interleaved operations"),
    "P18": ("Milestone", "State-based", "Enable task only after milestone reached"),
    "P19": ("Cancel Activity", "Cancellation", "Cancel a specific activity"),
    "P20": ("Cancel Case", "Cancellation", "Cancel entire workflow case"),
    "P21": ("Structured Loop", "Advanced Control Flow", "Repeat a set of activities"),
    "P22": ("Recursion", "Advanced Control Flow", "Workflow invokes itself"),
    "P23": ("Transient Trigger", "Trigger", "Trigger enabled only during specific activity"),
    "P24": ("Persistent Trigger", "Trigger", "Trigger remains enabled until consumed"),
    "P25": ("Cancel Region", "Cancellation", "Cancel a region of activities"),
    "P26": ("Cancel MI Activity", "Multiple Instance", "Cancel all instances of an activity"),
    "P27": ("Complete MI Activity", "Multiple Instance", "Complete all instances early if threshold met"),
    "P28": ("Blocking Discriminator", "Advanced Synchronization", "Discriminator that blocks until first arrival"),
    "P29": ("Cancelling Discriminator", "Advanced Synchronization", "Discriminator that cancels other branches"),
    "P30": ("Structured Partial Join", "Advanced Synchronization", "Wait for N of M branches"),
    "P31": ("Blocking Partial Join", "Advanced Synchronization", "Partial join with blocking behavior"),
    "P32": ("Cancelling Partial Join", "Advanced Synchronization", "Partial join that cancels remaining"),
    "P33": ("Generalized AND-Join", "Advanced Synchronization", "AND-join with N of M semantics"),
    "P34": ("Static Partial Join for MI", "Multiple Instance", "Static N of M for multiple instances"),
    "P35": ("Cancelling Partial Join MI", "Multiple Instance", "Cancel remaining instances after threshold"),
    "P36": ("Dynamic Partial Join for MI", "Multiple Instance", "Dynamic N of M for multiple instances"),
    "P37": ("Local Synchronizing Merge", "Advanced Merge", "Merge with local scope synchronization"),
    "P38": ("General Synchronizing Merge", "Advanced Merge", "General merge with synchronization"),
    "P39": ("Critical Section", "Concurrency", "Mutex for shared resource access"),
    "P40": ("Interleaved Routing", "Concurrency", "Interleaved execution pattern"),
    "P41": ("Thread Merge", "Thread Patterns", "Merge parallel execution threads"),
    "P42": ("Thread Split", "Thread Patterns", "Split into parallel execution threads"),
    "P43": ("Explicit Termination", "Termination", "Explicitly terminate workflow"),
}


@dataclass
class PatternCoverageReport:
    """Report on pattern coverage across the codebase."""
    total_patterns: int = 43
    patterns_found: Dict[str, CheckStatus] = field(default_factory=dict)
    files_scanned: int = 0
    pattern_definitions: Dict[str, List[str]] = field(default_factory=dict)
    pattern_references: Dict[str, List[str]] = field(default_factory=dict)

    def add_pattern(self, pattern_id: str, status: CheckStatus, location: str):
        """Add a pattern finding."""
        self.patterns_found[pattern_id] = status
        if status == CheckStatus.PRESENT:
            if pattern_id not in self.pattern_definitions:
                self.pattern_definitions[pattern_id] = []
            self.pattern_definitions[pattern_id].append(location)
        elif status == CheckStatus.REFERENCED:
            if pattern_id not in self.pattern_references:
                self.pattern_references[pattern_id] = []
            self.pattern_references[pattern_id].append(location)

    def get_coverage_percentage(self) -> float:
        """Calculate coverage percentage."""
        if self.total_patterns == 0:
            return 0.0
        present = sum(1 for s in self.patterns_found.values()
                     if s in (CheckStatus.PRESENT, CheckStatus.REFERENCED))
        return (present / self.total_patterns) * 100

    def to_dict(self) -> dict:
        """Convert report to dictionary for JSON serialization."""
        missing = [pid for pid, status in self.patterns_found.items()
                  if status == CheckStatus.MISSING]
        present = [pid for pid, status in self.patterns_found.items()
                  if status == CheckStatus.PRESENT]

        return {
            "summary": {
                "total_patterns": self.total_patterns,
                "patterns_present": len(present),
                "patterns_missing": len(missing),
                "coverage_percent": round(self.get_coverage_percentage(), 2),
                "files_scanned": self.files_scanned
            },
            "patterns": {
                pid: {
                    "name": YAWL_PATTERNS[pid][0],
                    "category": YAWL_PATTERNS[pid][1],
                    "description": YAWL_PATTERNS[pid][2],
                    "status": self.patterns_found.get(pid, "MISSING"),
                    "defined_in": self.pattern_definitions.get(pid, []),
                    "referenced_in": self.pattern_references.get(pid, [])
                }
                for pid in YAWL_PATTERNS.keys()
            },
            "missing_patterns": missing
        }


class PatternCoverageChecker:
    """Checker for YAWL pattern coverage."""

    # Regex patterns for finding patterns
    MODULE_NAME_PATTERN = re.compile(r'^-module\((\w+)\)', re.MULTILINE)
    PATTERN_ID_PATTERN = re.compile(r'\bP(\d{1,2})\b')
    PATTERN_EXPORT_PATTERN = re.compile(
        r'pattern_module\s*\(\s*<<"?P(\d{1,2})',
        re.MULTILINE
    )
    PATTERN_REGISTRY_PATTERN = re.compile(
        r'pattern_module\s*\(\s*<<"?(P\d{1,2}[_\w]*)"?\s*\)\s*->\s*(\w+)',
        re.MULTILINE
    )

    def __init__(self, base_path: str):
        """Initialize checker with base path to scan.

        Args:
            base_path: Root directory to scan for pattern files
        """
        self.base_path = Path(base_path)
        self.report = PatternCoverageReport()

    def scan(self) -> PatternCoverageReport:
        """Scan the codebase for pattern usage.

        Returns:
            Coverage report
        """
        # First, check pattern registry if it exists
        registry_path = self.base_path / "src/core/yawl_pattern_registry.erl"
        if registry_path.exists():
            self._scan_pattern_registry(registry_path)

        # Scan pattern modules
        patterns_dir = self.base_path / "src/patterns"
        if patterns_dir.exists():
            self._scan_pattern_modules(patterns_dir)

        # Scan for pattern references in other files
        self._scan_for_references()

        return self.report

    def _scan_pattern_registry(self, registry_path: Path):
        """Scan the pattern registry module."""
        try:
            content = registry_path.read_text(encoding='utf-8')
            self.report.files_scanned += 1

            # Find all pattern_module mappings
            for match in self.PATTERN_REGISTRY_PATTERN.finditer(content):
                pattern_id = match.group(1)
                module_name = match.group(2)

                # Extract just P<number> from pattern_id
                pid_match = re.match(r'P(\d+)', pattern_id)
                if pid_match:
                    pid = f"P{pid_match.group(1)}"
                    if pid in YAWL_PATTERNS:
                        self.report.add_pattern(
                            pid,
                            CheckStatus.PRESENT,
                            f"Defined in registry -> {module_name}"
                        )
        except IOError:
            pass

    def _scan_pattern_modules(self, patterns_dir: Path):
        """Scan the patterns directory for pattern implementations."""
        for erl_file in patterns_dir.glob("*.erl"):
            try:
                content = erl_file.read_text(encoding='utf-8')
                self.report.files_scanned += 1

                # Get module name
                module_match = self.MODULE_NAME_PATTERN.search(content)
                module_name = module_match.group(1) if module_match else erl_file.stem

                # Look for pattern identifiers in comments or exports
                for pid in YAWL_PATTERNS.keys():
                    # Check if filename suggests this pattern
                    if pid.lower() in erl_file.name.lower():
                        self.report.add_pattern(
                            pid,
                            CheckStatus.PRESENT,
                            f"Module: {module_name} ({erl_file.name})"
                        )
                    # Check for pattern ID in exports or documentation
                    elif re.search(rf'"{pid}"', content) or re.search(rf"'{pid}'", content):
                        self.report.add_pattern(
                            pid,
                            CheckStatus.PRESENT,
                            f"Module: {module_name} ({erl_file.name})"
                        )
            except IOError:
                pass

    def _scan_for_references(self):
        """Scan all Erlang files for pattern references."""
        erl_files = list(self.base_path.rglob("*.erl"))

        for erl_file in erl_files:
            if "patterns" in erl_file.parts:
                continue  # Already scanned

            try:
                content = erl_file.read_text(encoding='utf-8')

                # Look for pattern references
                for pid in YAWL_PATTERNS.keys():
                    # Look for pattern strings like <<"P1_Sequence">>
                    pattern_ref = rf'"{pid}[_\w]*"|\'{pid}[_\w]*\'|<<\"?{pid}[_\w]*\"?>>'
                    if re.search(pattern_ref, content):
                        if pid not in self.report.pattern_definitions:
                            self.report.add_pattern(
                                pid,
                                CheckStatus.REFERENCED,
                                str(erl_file.relative_to(self.base_path))
                            )
            except IOError:
                pass

        # Mark remaining patterns as missing
        for pid in YAWL_PATTERNS.keys():
            if pid not in self.report.patterns_found:
                self.report.patterns_found[pid] = CheckStatus.MISSING


def print_text_report(report: PatternCoverageReport) -> int:
    """Print coverage report in human-readable format.

    Returns:
        Exit code (0 if all present, 1 if missing)
    """
    print(f"\n{'=' * 70}")
    print(f"YAWL Pattern Coverage Report")
    print(f"{'=' * 70}\n")

    coverage = report.get_coverage_percentage()
    present = sum(1 for s in report.patterns_found.values()
                if s in (CheckStatus.PRESENT, CheckStatus.REFERENCED))
    missing = report.total_patterns - present

    print(f"Coverage: {present}/{report.total_patterns} patterns ({coverage:.1f}%)")
    print(f"Files scanned: {report.files_scanned}\n")

    if missing == 0:
        print("All 43 YAWL patterns are present!")
    else:
        print(f"Missing {missing} pattern(s):\n")
        for pid in sorted(YAWL_PATTERNS.keys()):
            status = report.patterns_found.get(pid)
            if status != CheckStatus.PRESENT and status != CheckStatus.REFERENCED:
                name, category, _ = YAWL_PATTERNS[pid]
                print(f"  [{pid}] {name:30} ({category})")

    print("\nPattern status by category:\n")

    # Group by category
    by_category = {}
    for pid, (name, category, _) in YAWL_PATTERNS.items():
        if category not in by_category:
            by_category[category] = []
        status = report.patterns_found.get(pid, CheckStatus.MISSING)
        symbol = "X" if status in (CheckStatus.PRESENT, CheckStatus.REFERENCED) else " "
        by_category[category].append((pid, name, symbol))

    for category, patterns in sorted(by_category.items()):
        print(f"{category}:")
        for pid, name, symbol in sorted(patterns):
            print(f"  [{symbol}] {pid:4} {name}")
        print()

    return 0 if missing == 0 else 1


def print_json_report(report: PatternCoverageReport):
    """Print coverage report in JSON format."""
    print(json.dumps(report.to_dict(), indent=2))


def print_tap_report(report: PatternCoverageReport):
    """Print coverage report in TAP format."""
    print("TAP version 13")
    print(f"1..{report.total_patterns}")

    for i, (pid, (name, category, _)) in enumerate(sorted(YAWL_PATTERNS.items()), 1):
        status = report.patterns_found.get(pid, CheckStatus.MISSING)
        ok = status in (CheckStatus.PRESENT, CheckStatus.REFERENCED)
        print(f"{'ok' if ok else 'not ok'} {i} - {pid} {name} ({category})")
        if not ok:
            print(f"  # Missing pattern implementation")

    # Diagnostic line
    print(f"# Coverage: {report.get_coverage_percentage():.1f}%")


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Verify all 43 YAWL patterns are implemented"
    )
    parser.add_argument(
        "path",
        nargs="?",
        default=".",
        help="Path to CRE source directory (default: current directory)"
    )
    parser.add_argument(
        "--json", "-j",
        action="store_true",
        help="Output in JSON format"
    )
    parser.add_argument(
        "--tap",
        action="store_true",
        help="Output in TAP format"
    )
    parser.add_argument(
        "--output", "-o",
        help="Write output to file"
    )

    args = parser.parse_args()

    path = Path(args.path)
    if not path.exists():
        print(f"Error: Path not found: {args.path}", file=sys.stderr)
        return 2

    checker = PatternCoverageChecker(str(path))
    report = checker.scan()

    # Generate output
    if args.tap:
        output_lines = []
        from io import StringIO
        old_stdout = sys.stdout
        sys.stdout = StringIO()
        print_tap_report(report)
        output = sys.stdout.getvalue()
        sys.stdout = old_stdout
        print(output, end="")
    elif args.json:
        output = json.dumps(report.to_dict(), indent=2)
        print(output)
    else:
        exit_code = print_text_report(report)

    # Write to file if requested
    if args.output:
        with open(args.output, 'w', encoding='utf-8') as f:
            if args.tap:
                old_stdout = sys.stdout
                sys.stdout = StringIO()
                print_tap_report(report)
                f.write(sys.stdout.getvalue())
                sys.stdout = old_stdout
            elif args.json:
                f.write(json.dumps(report.to_dict(), indent=2))
            else:
                from io import StringIO
                old_stdout = sys.stdout
                sys.stdout = StringIO()
                print_text_report(report)
                f.write(sys.stdout.getvalue())
                sys.stdout = old_stdout

    return 0 if report.get_coverage_percentage() == 100 else 1


if __name__ == "__main__":
    sys.exit(main())
