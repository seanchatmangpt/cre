#!/usr/bin/env python3
"""
verify_receipts.py - Analyze receipt chains for completeness

This script analyzes workflow receipt chains (from wf_audit_log or similar)
to verify completeness, consistency, and correctness of the execution trail.
Receipts record state transitions in Petri net-based workflows.

Usage:
    python verify_receipts.py path/to/receipts.log
    python verify_receipts.py --json path/to/receipts.log
    python verify_receipts.py --directory /var/log/cre/

Receipt format (Erlang term):
    #{
        before_hash := binary(),
        after_hash := binary(),
        move := #{
            trsn := atom(),
            mode := #{place() => [token()]},
            produce := #{place() => [token()]}
        },
        ts := integer()
    }

Exit codes:
    0: Receipt chain valid
    1: Validation errors found
    2: File access errors
"""

import argparse
import hashlib
import json
import re
import sys
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List, Optional, Set, Tuple


class CheckStatus(Enum):
    """Status of a receipt check."""
    PASS = "PASS"
    FAIL = "FAIL"
    WARN = "WARN"


@dataclass
class Receipt:
    """A workflow execution receipt."""
    before_hash: str
    after_hash: str
    move: Dict[str, Any]
    timestamp: int
    raw_data: str = ""
    line_number: int = 0

    def __hash__(self):
        return hash((self.before_hash, self.after_hash, self.timestamp))

    def __eq__(self, other):
        if not isinstance(other, Receipt):
            return False
        return (self.before_hash == other.before_hash and
                self.after_hash == other.after_hash and
                self.timestamp == other.timestamp)


@dataclass
class ChainViolation:
    """A violation in the receipt chain."""
    violation_type: str
    description: str
    receipt_index: int
    severity: str = "error"  # error, warning


@dataclass
class ReceiptAnalysisReport:
    """Report from receipt chain analysis."""
    file_path: str
    total_receipts: int = 0
    valid_receipts: int = 0
    violations: List[ChainViolation] = field(default_factory=list)
    chain_complete: bool = False
    first_timestamp: Optional[int] = None
    last_timestamp: Optional[int] = None
    unique_transitions: Set[str] = field(default_factory=set)
    hash_chain_broken: bool = False

    def add_violation(self, violation_type: str, description: str,
                     receipt_index: int, severity: str = "error"):
        """Add a violation to the report."""
        self.violations.append(ChainViolation(
            violation_type=violation_type,
            description=description,
            receipt_index=receipt_index,
            severity=severity
        ))

    def to_dict(self) -> Dict[str, Any]:
        """Convert report to dictionary for JSON serialization."""
        errors = [v for v in self.violations if v.severity == "error"]
        warnings = [v for v in self.violations if v.severity == "warning"]

        return {
            "file": self.file_path,
            "summary": {
                "total_receipts": self.total_receipts,
                "valid_receipts": self.valid_receipts,
                "violations": {
                    "total": len(self.violations),
                    "errors": len(errors),
                    "warnings": len(warnings)
                },
                "chain_complete": self.chain_complete,
                "hash_chain_intact": not self.hash_chain_broken,
                "time_span_ms": (
                    self.last_timestamp - self.first_timestamp
                    if self.first_timestamp and self.last_timestamp
                    else None
                ),
                "unique_transitions": len(self.unique_transitions)
            },
            "violations": [
                {
                    "type": v.violation_type,
                    "description": v.description,
                    "receipt_index": v.receipt_index,
                    "severity": v.severity
                }
                for v in self.violations
            ],
            "unique_transitions": sorted(self.unique_transitions),
            "overall": "PASS" if not errors else "FAIL"
        }


class ReceiptParser:
    """Parser for Erlang-style receipt logs."""

    # Pattern to match Erlang map syntax
    RECEIPT_PATTERN = re.compile(
        r'#?\{\s*'
        r'before_hash\s*=>\s*<<([^>]+)>>\s*,\s*'
        r'after_hash\s*=>\s*<<([^>]+)>>\s*,\s*'
        r'move\s*=>\s*#\{[^}]*\}\s*,\s*'
        r'ts\s*=>\s*(\d+)'  # Simplified - real parser would be more complex
        r'\s*\}',
        re.MULTILINE
    )

    # Alternative pattern for different formatting
    RECEIPT_PATTERN_ALT = re.compile(
        r'\{.*?before_hash.*?<<([^>]+)>>.*?after_hash.*?<<([^>]+)>>.*?ts.*?(\d+)',
        re.DOTALL
    )

    # Binary hash pattern (<<...>>)
    BINARY_PATTERN = re.compile(r'<<(\d+|\d+,\s*\d+(?:,\s*\d+)*)>>')

    def __init__(self, content: str, file_path: str):
        """Initialize parser with log content.

        Args:
            content: Raw log file content
            file_path: Path to the log file
        """
        self.content = content
        self.file_path = file_path

    def parse(self) -> List[Receipt]:
        """Parse receipts from log content.

        Returns:
            List of parsed Receipt objects
        """
        receipts = []
        lines = self.content.split('\n')

        for i, line in enumerate(lines, start=1):
            line = line.strip()
            if not line or line.startswith('%') or line.startswith('#'):
                continue

            receipt = self._parse_line(line, i)
            if receipt:
                receipts.append(receipt)

        return receipts

    def _parse_line(self, line: str, line_num: int) -> Optional[Receipt]:
        """Parse a single receipt line.

        This is a simplified parser that looks for the key fields.
        A full implementation would use a proper Erlang term parser.
        """
        try:
            # Try to extract before_hash
            before_match = re.search(r'before_hash\s*=>\s*<<([^>]+)>>', line)
            if not before_match:
                before_match = re.search(r'"before_hash"\s*:\s*"([^"]+)"', line)
            if not before_match:
                return None

            # Try to extract after_hash
            after_match = re.search(r'after_hash\s*=>\s*<<([^>]+)>>', line)
            if not after_match:
                after_match = re.search(r'"after_hash"\s*:\s*"([^"]+)"', line)
            if not after_match:
                return None

            # Try to extract timestamp
            ts_match = re.search(r'\bts\s*=>\s*(\d+)', line)
            if not ts_match:
                ts_match = re.search(r'"ts"\s*:\s*(\d+)', line)
            if not ts_match:
                return None

            # Try to extract transition name
            trsn_match = re.search(r'trsn\s*=>\s*(\w+)', line)
            if not trsn_match:
                trsn_match = re.search(r'"trsn"\s*:\s*"(\w+)"', line)
            trsn = trsn_match.group(1) if trsn_match else "unknown"

            return Receipt(
                before_hash=before_match.group(1),
                after_hash=after_match.group(1),
                move={"trsn": trsn},
                timestamp=int(ts_match.group(1)),
                raw_data=line,
                line_number=line_num
            )
        except Exception:
            return None


class ReceiptChainAnalyzer:
    """Analyzer for receipt chain validation."""

    def __init__(self, receipts: List[Receipt], file_path: str):
        """Initialize analyzer with receipt list.

        Args:
            receipts: List of parsed receipts
            file_path: Path to source file
        """
        self.receipts = sorted(receipts, key=lambda r: r.timestamp)
        self.file_path = file_path
        self.report = ReceiptAnalysisReport(file_path=file_path)

    def analyze(self) -> ReceiptAnalysisReport:
        """Run all chain validation checks.

        Returns:
            Complete analysis report
        """
        self.report.total_receipts = len(self.receipts)

        if not self.receipts:
            self.report.add_violation(
                "EMPTY_CHAIN",
                "No receipts found in file",
                0,
                "error"
            )
            return self.report

        # Set timestamps
        self.report.first_timestamp = self.receipts[0].timestamp
        self.report.last_timestamp = self.receipts[-1].timestamp

        # Collect unique transitions
        for r in self.receipts:
            if "trsn" in r.move:
                self.report.unique_transitions.add(r.move["trsn"])

        # Validate each receipt
        for i, receipt in enumerate(self.receipts):
            if self._validate_receipt_structure(receipt, i):
                self.report.valid_receipts += 1

        # Check chain continuity
        self._check_hash_chain()

        # Check for duplicate receipts
        self._check_duplicates()

        # Check timestamp ordering
        self._check_timestamps()

        # Determine if chain is complete
        self.report.chain_complete = (
            not self.report.hash_chain_broken and
            len(self.receipts) > 0
        )

        return self.report

    def _validate_receipt_structure(self, receipt: Receipt, index: int) -> bool:
        """Validate individual receipt structure.

        Returns:
            True if receipt is valid
        """
        valid = True

        # Check before_hash is non-empty
        if not receipt.before_hash:
            self.report.add_violation(
                "MISSING_BEFORE_HASH",
                "Receipt missing before_hash",
                index,
                "error"
            )
            valid = False

        # Check after_hash is non-empty
        if not receipt.after_hash:
            self.report.add_violation(
                "MISSING_AFTER_HASH",
                "Receipt missing after_hash",
                index,
                "error"
            )
            valid = False

        # Check timestamp is reasonable
        if receipt.timestamp <= 0:
            self.report.add_violation(
                "INVALID_TIMESTAMP",
                f"Invalid timestamp: {receipt.timestamp}",
                index,
                "error"
            )
            valid = False

        # Check move structure
        if not receipt.move or "trsn" not in receipt.move:
            self.report.add_violation(
                "MISSING_TRANSITION",
                "Receipt missing transition name in move",
                index,
                "warning"
            )

        return valid

    def _check_hash_chain(self):
        """Verify that hash chain is continuous."""
        for i in range(len(self.receipts) - 1):
            current = self.receipts[i]
            next_receipt = self.receipts[i + 1]

            # The after_hash of current should match before_hash of next
            if current.after_hash != next_receipt.before_hash:
                self.report.add_violation(
                    "HASH_CHAIN_BREAK",
                    f"Hash chain break at index {i}: "
                    f"{current.after_hash} != {next_receipt.before_hash}",
                    i,
                    "error"
                )
                self.report.hash_chain_broken = True

    def _check_duplicates(self):
        """Check for duplicate receipts."""
        seen = {}
        for i, receipt in enumerate(self.receipts):
            key = (receipt.before_hash, receipt.after_hash, receipt.timestamp)
            if key in seen:
                self.report.add_violation(
                    "DUPLICATE_RECEIPT",
                    f"Duplicate receipt (first seen at index {seen[key]})",
                    i,
                    "warning"
                )
            seen[key] = i

    def _check_timestamps(self):
        """Check timestamp monotonicity."""
        for i in range(len(self.receipts) - 1):
            current = self.receipts[i]
            next_receipt = self.receipts[i + 1]

            if next_receipt.timestamp < current.timestamp:
                self.report.add_violation(
                    "TIMESTAMP_OUT_OF_ORDER",
                    f"Timestamp goes backward: {current.timestamp} -> {next_receipt.timestamp}",
                    i + 1,
                    "warning"
                )


def print_text_report(report: ReceiptAnalysisReport) -> int:
    """Print analysis report in human-readable format.

    Returns:
        Exit code (0 for pass, 1 for failure)
    """
    print(f"\n{'=' * 70}")
    print(f"Receipt Chain Analysis: {report.file_path}")
    print(f"{'=' * 70}\n")

    print(f"Total receipts: {report.total_receipts}")
    print(f"Valid receipts: {report.valid_receipts}")

    if report.first_timestamp and report.last_timestamp:
        duration_ms = report.last_timestamp - report.first_timestamp
        duration_s = duration_ms / 1000
        print(f"Time span: {duration_ms} ms ({duration_s:.2f} seconds)")

    print(f"Unique transitions: {len(report.unique_transitions)}")
    if report.unique_transitions:
        print(f"Transitions: {', '.join(sorted(report.unique_transitions))}")

    errors = [v for v in report.violations if v.severity == "error"]
    warnings = [v for v in report.violations if v.severity == "warning"]

    print(f"\nViolations: {len(errors)} errors, {len(warnings)} warnings")

    if errors:
        print("\nERRORS:")
        for v in errors:
            print(f"  [{v.receipt_index}] {v.violation_type}: {v.description}")

    if warnings:
        print("\nWARNINGS:")
        for v in warnings:
            print(f"  [{v.receipt_index}] {v.violation_type}: {v.description}")

    print("\nChain Status:")
    print(f"  Complete: {report.chain_complete}")
    print(f"  Hash chain intact: {not report.hash_chain_broken}")

    if report.chain_complete and not report.hash_chain_broken:
        print("\nOverall: RECEIPT CHAIN VALID")
        return 0
    else:
        print("\nOverall: RECEIPT CHAIN INVALID")
        return 1


def print_json_report(report: ReceiptAnalysisReport):
    """Print analysis report in JSON format."""
    print(json.dumps(report.to_dict(), indent=2))


def print_tap_report(report: ReceiptAnalysisReport, test_number: int) -> int:
    """Print analysis report in TAP format.

    Returns:
        Next test number
    """
    errors = [v for v in report.violations if v.severity == "error"]

    print(f"{'ok' if not errors else 'not ok'} {test_number} - Receipt chain: {report.file_path}")

    for v in report.violations:
        severity = "Failed" if v.severity == "error" else "Warning"
        print(f"  # {severity} [{v.receipt_index}]: {v.violation_type} - {v.description}")

    return test_number + 1


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Analyze receipt chains for completeness"
    )
    parser.add_argument(
        "path",
        help="Path to receipt log file or directory containing logs"
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

    # Find log files
    if path.is_dir():
        log_files = list(path.rglob("*.log")) + list(path.rglob("receipts*"))
    else:
        log_files = [path]

    if not log_files:
        print("Error: No log files found", file=sys.stderr)
        return 2

    all_reports = []
    exit_code = 0

    for log_file in log_files:
        try:
            content = log_file.read_text(encoding='utf-8', errors='ignore')
        except IOError as e:
            print(f"Error reading file {log_file}: {e}", file=sys.stderr)
            exit_code = 2
            continue

        parser = ReceiptParser(content, str(log_file))
        receipts = parser.parse()

        analyzer = ReceiptChainAnalyzer(receipts, str(log_file))
        report = analyzer.analyze()
        all_reports.append(report)

        if any(v.severity == "error" for v in report.violations):
            exit_code = 1

    # Generate output
    if args.tap:
        print("TAP version 13")
        print(f"1..{len(all_reports)}")
        test_num = 1
        for report in all_reports:
            test_num = print_tap_report(report, test_num)
    elif args.json:
        if len(all_reports) == 1:
            print(json.dumps(all_reports[0].to_dict(), indent=2))
        else:
            print(json.dumps(
                {"reports": [r.to_dict() for r in all_reports]},
                indent=2
            ))
    else:
        if len(all_reports) == 1:
            exit_code = print_text_report(all_reports[0])
        else:
            print(f"\nReceipt Chain Analysis: {len(all_reports)} files\n")
            for report in all_reports:
                errors = sum(1 for v in report.violations if v.severity == "error")
                warnings = sum(1 for v in report.violations if v.severity == "warning")
                status = "OK" if errors == 0 else "FAIL"
                print(f"  [{status}] {report.file_path}: "
                      f"{report.total_receipts} receipts, "
                      f"{errors} errors, {warnings} warnings")
            print()

    # Write to file if requested
    if args.output:
        with open(args.output, 'w', encoding='utf-8') as f:
            if args.tap:
                from io import StringIO
                old_stdout = sys.stdout
                sys.stdout = StringIO()
                print("TAP version 13")
                print(f"1..{len(all_reports)}")
                test_num = 1
                for report in all_reports:
                    test_num = print_tap_report(report, test_num)
                f.write(sys.stdout.getvalue())
                sys.stdout = old_stdout
            elif args.json:
                if len(all_reports) == 1:
                    f.write(json.dumps(all_reports[0].to_dict(), indent=2))
                else:
                    f.write(json.dumps(
                        {"reports": [r.to_dict() for r in all_reports]},
                        indent=2
                    ))
            else:
                from io import StringIO
                old_stdout = sys.stdout
                sys.stdout = StringIO()
                for report in all_reports:
                    print_text_report(report)
                f.write(sys.stdout.getvalue())
                sys.stdout = old_stdout

        print(f"\nOutput written to: {args.output}")

    return exit_code


if __name__ == "__main__":
    sys.exit(main())
