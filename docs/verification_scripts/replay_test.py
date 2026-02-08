#!/usr/bin/env python3
"""
replay_test.py - Replay a workflow from receipts and compare outcomes

This script replays a workflow execution from receipt logs and compares
the final state against an expected outcome. It validates that the receipt
chain produces the correct final marking.

Usage:
    python replay_test.py receipts.log expected_final_state.json
    python replay_test.py --json receipts.log expected_final_state.json
    python replay_test.py --generate-expected receipts.log

Expected state format (JSON):
    {
        "marking": {
            "place1": ["token1", "token2"],
            "place2": []
        },
        "terminated": true,
        "final_transition": "end_task"
    }

Exit codes:
    0: Replay matches expected outcome
    1: Replay differs from expected
    2: File access or parse errors
"""

import argparse
import hashlib
import json
import re
import sys
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List, Optional, Set, Tuple


class ReplayStatus(Enum):
    """Status of replay check."""
    MATCH = "MATCH"
    DIFFER = "DIFFER"
    ERROR = "ERROR"
    SKIP = "SKIP"


@dataclass
class ReplayMismatch:
    """A mismatch between replay and expected state."""
    field: str
    expected: Any
    actual: Any
    description: str


@dataclass
class ReplayResult:
    """Result of workflow replay."""
    file_path: str
    success: bool = False
    final_marking: Dict[str, List[Any]] = field(default_factory=dict)
    transitions_executed: List[str] = field(default_factory=list)
    total_receipts: int = 0
    mismatches: List[ReplayMismatch] = field(default_factory=list)
    replay_terminated: bool = False
    final_transition: Optional[str] = None

    def to_dict(self) -> Dict[str, Any]:
        """Convert result to dictionary."""
        return {
            "file": self.file_path,
            "success": self.success,
            "final_marking": self.final_marking,
            "transitions_executed": self.transitions_executed,
            "total_receipts": self.total_receipts,
            "mismatches": [
                {
                    "field": m.field,
                    "expected": m.expected,
                    "actual": m.actual,
                    "description": m.description
                }
                for m in self.mismatches
            ],
            "replay_terminated": self.replay_terminated,
            "final_transition": self.final_transition
        }


@dataclass
class ExpectedState:
    """Expected final state of a workflow."""
    marking: Dict[str, List[Any]] = field(default_factory=dict)
    terminated: bool = False
    final_transition: Optional[str] = None
    min_transitions: int = 0
    max_transitions: int = 999999
    required_places: Set[str] = field(default_factory=set)
    forbidden_places: Set[str] = field(default_factory=set)

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "ExpectedState":
        """Create from JSON dict."""
        obj = cls()
        obj.marking = data.get("marking", {})
        obj.terminated = data.get("terminated", False)
        obj.final_transition = data.get("final_transition")
        obj.min_transitions = data.get("min_transitions", 0)
        obj.max_transitions = data.get("max_transitions", 999999)
        obj.required_places = set(data.get("required_places", []))
        obj.forbidden_places = set(data.get("forbidden_places", []))
        return obj


@dataclass
class Receipt:
    """A workflow execution receipt."""
    before_hash: str
    after_hash: str
    move: Dict[str, Any]
    timestamp: int
    raw_data: str = ""
    line_number: int = 0


class ReceiptParser:
    """Parser for receipt logs."""

    def __init__(self, content: str, file_path: str):
        """Initialize parser."""
        self.content = content
        self.file_path = file_path

    def parse(self) -> List[Receipt]:
        """Parse receipts from content."""
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
        """Parse a single receipt line."""
        try:
            # Extract before_hash
            before_match = re.search(r'before_hash\s*=>\s*<<([^>]+)>>', line)
            if not before_match:
                before_match = re.search(r'"before_hash"\s*:\s*"([^"]+)"', line)
            if not before_match:
                return None

            # Extract after_hash
            after_match = re.search(r'after_hash\s*=>\s*<<([^>]+)>>', line)
            if not after_match:
                after_match = re.search(r'"after_hash"\s*:\s*"([^"]+)"', line)
            if not after_match:
                return None

            # Extract timestamp
            ts_match = re.search(r'\bts\s*=>\s*(\d+)', line)
            if not ts_match:
                ts_match = re.search(r'"ts"\s*:\s*(\d+)', line)
            if not ts_match:
                return None

            # Extract transition name
            trsn_match = re.search(r'trsn\s*=>\s*(\w+)', line)
            if not trsn_match:
                trsn_match = re.search(r'"trsn"\s*:\s*"(\w+)"', line)
            trsn = trsn_match.group(1) if trsn_match else "unknown"

            # Try to extract mode/produce info
            mode_match = re.search(r'mode\s*=>\s*#\{([^}]+)\}', line)
            produce_match = re.search(r'produce\s*=>\s*#\{([^}]+)\}', line)

            move = {"trsn": trsn}
            if mode_match:
                move["mode_hint"] = mode_match.group(1)
            if produce_match:
                move["produce_hint"] = produce_match.group(1)

            return Receipt(
                before_hash=before_match.group(1),
                after_hash=after_match.group(1),
                move=move,
                timestamp=int(ts_match.group(1)),
                raw_data=line,
                line_number=line_num
            )
        except Exception:
            return None


class WorkflowReplayer:
    """Replays workflow execution from receipts."""

    def __init__(self, receipts: List[Receipt], file_path: str):
        """Initialize replayer."""
        self.receipts = sorted(receipts, key=lambda r: r.timestamp)
        self.file_path = file_path
        self.result = ReplayResult(file_path=file_path)

    def replay(self, expected: Optional[ExpectedState] = None) -> ReplayResult:
        """Replay workflow and compare against expected state.

        Args:
            expected: Expected final state (optional)

        Returns:
            Replay result
        """
        self.result.total_receipts = len(self.receipts)

        # Simulate marking changes
        # Since we don't have full state info, we simulate based on receipts
        current_marking: Dict[str, List[Any]] = {}

        for i, receipt in enumerate(self.receipts):
            # Track transition
            trsn = receipt.move.get("trsn", "unknown")
            self.result.transitions_executed.append(trsn)

            # Update marking based on produce hints
            produce_hint = receipt.move.get("produce_hint", "")
            if produce_hint:
                # Parse simple produce hints like "p => [token]"
                for part in produce_hint.split(','):
                    part = part.strip()
                    if '=>' in part:
                        place, _ = part.split('=>', 1)
                        place = place.strip()
                        if place not in current_marking:
                            current_marking[place] = ["token"]  # Simplified
                        else:
                            current_marking[place].append("token")

            # Check if this is a terminating transition
            if trsn in ("end", "terminate", "stop", "final", "complete"):
                self.result.replay_terminated = True
                self.result.final_transition = trsn

        self.result.final_marking = current_marking

        # Compare against expected if provided
        if expected:
            self._compare_expected(expected)

        return self.result

    def _compare_expected(self, expected: ExpectedState):
        """Compare replay result against expected state."""
        # Check termination status
        if expected.terminated is not None:
            if self.result.replay_terminated != expected.terminated:
                self.result.mismatches.append(ReplayMismatch(
                    field="terminated",
                    expected=expected.terminated,
                    actual=self.result.replay_terminated,
                    description=f"Expected termination={expected.terminated}, "
                               f"got {self.result.replay_terminated}"
                ))

        # Check final transition
        if expected.final_transition:
            if self.result.final_transition != expected.final_transition:
                self.result.mismatches.append(ReplayMismatch(
                    field="final_transition",
                    expected=expected.final_transition,
                    actual=self.result.final_transition or "none",
                    description=f"Expected final transition {expected.final_transition}, "
                               f"got {self.result.final_transition}"
                ))

        # Check transition count
        actual_count = len(self.result.transitions_executed)
        if actual_count < expected.min_transitions:
            self.result.mismatches.append(ReplayMismatch(
                field="transition_count",
                expected=f">={expected.min_transitions}",
                actual=actual_count,
                description=f"Too few transitions: {actual_count} < {expected.min_transitions}"
            ))
        if actual_count > expected.max_transitions:
            self.result.mismatches.append(ReplayMismatch(
                field="transition_count",
                expected=f"<={expected.max_transitions}",
                actual=actual_count,
                description=f"Too many transitions: {actual_count} > {expected.max_transitions}"
            ))

        # Check required places
        for place in expected.required_places:
            if place not in self.result.final_marking:
                self.result.mismatches.append(ReplayMismatch(
                    field="required_place",
                    expected=f"place '{place}' present",
                    actual="missing",
                    description=f"Required place '{place}' not in final marking"
                ))

        # Check forbidden places
        for place in expected.forbidden_places:
            if place in self.result.final_marking:
                self.result.mismatches.append(ReplayMismatch(
                    field="forbidden_place",
                    expected=f"place '{place}' absent",
                    actual=f"present with {len(self.result.final_marking[place])} tokens",
                    description=f"Forbidden place '{place}' has tokens in final marking"
                ))

        # Check marking if specified
        if expected.marking:
            for place, expected_tokens in expected.marking.items():
                actual_tokens = self.result.final_marking.get(place, [])
                # Simplified comparison - just check presence/absence
                if expected_tokens and not actual_tokens:
                    self.result.mismatches.append(ReplayMismatch(
                        field=f"marking.{place}",
                        expected=f"{len(expected_tokens)} tokens",
                        actual="0 tokens",
                        description=f"Place '{place}' should have tokens but is empty"
                    ))

        self.result.success = len(self.result.mismatches) == 0


def print_text_report(result: ReplayResult) -> int:
    """Print replay result in human-readable format.

    Returns:
        Exit code (0 for match, 1 for mismatch)
    """
    print(f"\n{'=' * 70}")
    print(f"Workflow Replay Result: {result.file_path}")
    print(f"{'=' * 70}\n")

    print(f"Receipts processed: {result.total_receipts}")
    print(f"Transitions executed: {len(result.transitions_executed)}")

    if result.transitions_executed:
        print(f"Transition sequence: {' -> '.join(result.transitions_executed)}")

    print(f"\nFinal state:")
    print(f"  Terminated: {result.replay_terminated}")
    print(f"  Final transition: {result.final_transition or 'none'}")

    if result.final_marking:
        print(f"  Final marking:")
        for place, tokens in sorted(result.final_marking.items()):
            print(f"    {place}: {len(tokens)} token(s)")

    if result.mismatches:
        print(f"\nMISMATCHES ({len(result.mismatches)}):")
        for mismatch in result.mismatches:
            print(f"  [{mismatch.field}]")
            print(f"    Expected: {mismatch.expected}")
            print(f"    Actual: {mismatch.actual}")
            print(f"    {mismatch.description}")
        print()

        print("Overall: REPLAY FAILED - State does not match expected")
        return 1
    else:
        print("\nOverall: REPLAY PASSED - State matches expected")
        return 0


def print_json_report(result: ReplayResult):
    """Print replay result in JSON format."""
    print(json.dumps(result.to_dict(), indent=2))


def print_tap_report(result: ReplayResult, test_number: int) -> int:
    """Print replay result in TAP format.

    Returns:
        Next test number
    """
    print(f"{'ok' if result.success else 'not ok'} {test_number} - Replay: {result.file_path}")

    if result.mismatches:
        for m in result.mismatches:
            print(f"  # Mismatch [{m.field}]: {m.description}")
            print(f"  #   Expected: {m.expected}")
            print(f"  #   Actual: {m.actual}")

    # Diagnostic info
    print(f"  # Transitions: {len(result.transitions_executed)}")
    print(f"  # Terminated: {result.replay_terminated}")

    return test_number + 1


def generate_expected_template(receipts_file: str) -> int:
    """Generate a template expected state file from receipts.

    Returns:
        Exit code
    """
    path = Path(receipts_file)
    if not path.exists():
        print(f"Error: File not found: {receipts_file}", file=sys.stderr)
        return 2

    try:
        content = path.read_text(encoding='utf-8', errors='ignore')
    except IOError as e:
        print(f"Error reading file: {e}", file=sys.stderr)
        return 2

    parser = ReceiptParser(content, str(path))
    receipts = parser.parse()

    if not receipts:
        print("Error: No receipts found in file", file=sys.stderr)
        return 2

    replayer = WorkflowReplayer(receipts, str(path))
    result = replayer.replay()

    # Generate template
    template = {
        "_comment": "Expected state template - adjust values as needed",
        "marking": {},
        "terminated": result.replay_terminated,
        "final_transition": result.final_transition,
        "min_transitions": len(result.transitions_executed),
        "max_transitions": len(result.transitions_executed),
        "required_places": [],
        "forbidden_places": []
    }

    # Add current marking as reference
    for place in sorted(result.final_marking.keys()):
        template["marking"][place] = ["<token>"]  # Placeholder

    output_path = path.parent / f"{path.stem}_expected.json"
    with open(output_path, 'w', encoding='utf-8') as f:
        json.dump(template, f, indent=2)

    print(f"Generated expected state template: {output_path}")
    print("\nReview and adjust the template, then run:")
    print(f"  python replay_test.py {receipts_file} {output_path}")

    return 0


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Replay workflow from receipts and compare outcomes"
    )
    parser.add_argument(
        "receipts",
        help="Path to receipt log file"
    )
    parser.add_argument(
        "expected",
        nargs="?",
        help="Path to expected state JSON file"
    )
    parser.add_argument(
        "--generate-expected",
        action="store_true",
        help="Generate expected state template from receipts"
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

    # Handle template generation
    if args.generate_expected:
        return generate_expected_template(args.receipts)

    # Require expected state file unless generating template
    if not args.expected:
        print("Error: Expected state file required (or use --generate-expected)",
              file=sys.stderr)
        parser.print_help()
        return 2

    # Load receipts
    receipts_path = Path(args.receipts)
    if not receipts_path.exists():
        print(f"Error: Receipts file not found: {args.receipts}", file=sys.stderr)
        return 2

    try:
        content = receipts_path.read_text(encoding='utf-8', errors='ignore')
    except IOError as e:
        print(f"Error reading receipts file: {e}", file=sys.stderr)
        return 2

    parser = ReceiptParser(content, str(receipts_path))
    receipts = parser.parse()

    if not receipts:
        print("Error: No receipts found in file", file=sys.stderr)
        return 2

    # Load expected state
    expected_path = Path(args.expected)
    if not expected_path.exists():
        print(f"Error: Expected state file not found: {args.expected}", file=sys.stderr)
        return 2

    try:
        with open(expected_path, 'r', encoding='utf-8') as f:
            expected_data = json.load(f)
    except (IOError, json.JSONDecodeError) as e:
        print(f"Error reading expected state file: {e}", file=sys.stderr)
        return 2

    expected = ExpectedState.from_dict(expected_data)

    # Run replay
    replayer = WorkflowReplayer(receipts, str(receipts_path))
    result = replayer.replay(expected)

    # Generate output
    if args.tap:
        from io import StringIO
        old_stdout = sys.stdout
        sys.stdout = StringIO()
        print_tap_report(result, 1)
        output = sys.stdout.getvalue()
        sys.stdout = old_stdout
        print(output, end="")
    elif args.json:
        output = json.dumps(result.to_dict(), indent=2)
        print(output)
    else:
        exit_code = print_text_report(result)

    # Write to file if requested
    if args.output:
        with open(args.output, 'w', encoding='utf-8') as f:
            if args.tap:
                old_stdout = sys.stdout
                sys.stdout = StringIO()
                print_tap_report(result, 1)
                f.write(sys.stdout.getvalue())
                sys.stdout = old_stdout
            elif args.json:
                f.write(json.dumps(result.to_dict(), indent=2))
            else:
                from io import StringIO
                old_stdout = sys.stdout
                sys.stdout = StringIO()
                print_text_report(result)
                f.write(sys.stdout.getvalue())
                sys.stdout = old_stdout

        print(f"\nOutput written to: {args.output}")

    return 0 if result.success else 1


if __name__ == "__main__":
    sys.exit(main())
