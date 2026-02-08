#!/usr/bin/env python3
"""
validate_bcd.py - Parse and validate BCD (Business Context Document) structure

This script validates BCD markdown documents against the CRE BCD template structure.
It checks for required sections, proper formatting, and completeness of documentation.

Usage:
    python validate_bcd.py path/to/bcd.md
    python validate_bcd.py --json path/to/bcd.md
    python validate_bcd.py --directory docs/

Output formats:
    - Default: Human-readable text with pass/fail indicators
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
from typing import Any, Dict, List, Optional, Set, Tuple


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
    location: str = ""  # Line number or section reference


@dataclass
class BCDValidationReport:
    """Complete validation report for a BCD document."""
    file_path: str
    document_id: Optional[str] = None
    version: Optional[str] = None
    status: Optional[str] = None
    results: List[ValidationResult] = field(default_factory=list)
    sections_found: Set[str] = field(default_factory=set)
    required_sections: Set[str] = field(default_factory=set)

    def add_result(self, check_id: str, description: str,
                   status: CheckStatus, details: str = "", location: str = ""):
        """Add a validation result to the report."""
        self.results.append(ValidationResult(
            check_id=check_id,
            description=description,
            status=status,
            details=details,
            location=location
        ))

    def to_dict(self) -> Dict[str, Any]:
        """Convert report to dictionary for JSON serialization."""
        passed = sum(1 for r in self.results if r.status == CheckStatus.PASS)
        failed = sum(1 for r in self.results if r.status == CheckStatus.FAIL)
        warnings = sum(1 for r in self.results if r.status == CheckStatus.WARN)

        return {
            "file": self.file_path,
            "document_id": self.document_id,
            "version": self.version,
            "status": self.status,
            "summary": {
                "total": len(self.results),
                "passed": passed,
                "failed": failed,
                "warnings": warnings,
                "overall": "PASS" if failed == 0 else "FAIL"
            },
            "sections": {
                "found": sorted(self.sections_found),
                "required": sorted(self.required_sections),
                "missing": sorted(self.required_sections - self.sections_found)
            },
            "results": [
                {
                    "check_id": r.check_id,
                    "description": r.description,
                    "status": r.status.value,
                    "details": r.details,
                    "location": r.location
                }
                for r in self.results
            ]
        }


class BCDValidator:
    """Validator for Business Context Documents."""

    # Required sections in a BCD document
    REQUIRED_SECTIONS = {
        "Executive Summary",
        "Role Definitions",
        "Workflow Context",
        "Pattern Context",
        "Decision Schemas",
        "Traceability",
        "Non-Functional Requirements"
    }

    # Required header fields
    REQUIRED_HEADER_FIELDS = {
        "Document ID",
        "Version",
        "Status",
        "Created",
        "Last Modified",
        "Author"
    }

    # Valid status values
    VALID_STATUSES = {"Draft", "Review", "Approved", "Deprecated"}

    # Pattern ID regex (P1-P43)
    PATTERN_ID_RE = re.compile(r'\bP(\d{1,2})\b')

    # All 43 YAWL pattern IDs
    ALL_PATTERNS = {f"P{i}" for i in range(1, 44)}

    def __init__(self, content: str, file_path: str):
        """Initialize validator with BCD content.

        Args:
            content: The markdown content of the BCD
            file_path: Path to the BCD file (for error reporting)
        """
        self.content = content
        self.file_path = file_path
        self.lines = content.split('\n')
        self.report = BCDValidationReport(file_path=file_path)

    def validate(self) -> BCDValidationReport:
        """Run all validation checks.

        Returns:
            Complete validation report
        """
        # Parse document header
        self._validate_header()

        # Check required sections
        self._validate_sections()

        # Validate role definitions
        self._validate_roles()

        # Validate pattern context
        self._validate_patterns()

        # Validate traceability
        self._validate_traceability()

        # Validate decision schemas
        self._validate_decision_schemas()

        # Check for placeholders
        self._validate_no_placeholders()

        return self.report

    def _find_line_numbers(self, pattern: re.Pattern) -> List[Tuple[int, str]]:
        """Find all lines matching a pattern.

        Returns:
            List of (line_number, line_content) tuples (1-indexed)
        """
        matches = []
        for i, line in enumerate(self.lines, start=1):
            if pattern.search(line):
                matches.append((i, line))
        return matches

    def _get_section_lines(self, section_name: str) -> Tuple[int, int]:
        """Get the line range for a section.

        Returns:
            Tuple of (start_line, end_line) or (0, 0) if not found
        """
        start_pattern = re.compile(r'^#+\s*' + re.escape(section_name), re.IGNORECASE)
        start_line = 0
        end_line = len(self.lines)

        for i, line in enumerate(self.lines, start=1):
            if start_pattern.search(line):
                start_line = i
                break

        if start_line == 0:
            return (0, 0)

        # Find next section at same or higher level
        header_level = len(re.match(r'^(#+)', self.lines[start_line - 1]).group(1))

        for i in range(start_line, len(self.lines) + 1):
            if i > len(self.lines):
                break
            match = re.match(r'^(#+)\s', self.lines[i - 1])
            if match and len(match.group(1)) <= header_level:
                end_line = i - 1
                break

        return (start_line, end_line)

    def _validate_header(self):
        """Validate document header."""
        header_end = 0
        for i, line in enumerate(self.lines, start=1):
            if line.strip() == "---" and i > 1:
                header_end = i
                break

        if header_end == 0:
            self.report.add_result(
                "HDR_001",
                "Document header delimiter found",
                CheckStatus.FAIL,
                "No closing --- found in header",
                "Line 1"
            )
            return

        self.report.add_result(
            "HDR_001",
            "Document header delimiter found",
            CheckStatus.PASS,
            f"Header ends at line {header_end}"
        )

        # Extract header content
        header_lines = self.lines[1:header_end - 1]
        header_content = '\n'.join(header_lines)

        # Check document ID
        doc_id_match = re.search(r'\*\*Document ID:\*\*\s*(BCD-\d{8}-[\w-]+)', header_content)
        if doc_id_match:
            self.report.document_id = doc_id_match.group(1)
            self.report.add_result(
                "HDR_002",
                "Document ID format valid",
                CheckStatus.PASS,
                f"Found: {self.report.document_id}"
            )
        else:
            self.report.add_result(
                "HDR_002",
                "Document ID format valid",
                CheckStatus.FAIL,
                "Expected format: BCD-YYYYMMDD-WORKFLOW_ID",
                "Header"
            )

        # Check version
        version_match = re.search(r'\*\*Version:\*\*\s*(\d+\.\d+\.\d+)', header_content)
        if version_match:
            self.report.version = version_match.group(1)
            self.report.add_result(
                "HDR_003",
                "Version number valid",
                CheckStatus.PASS,
                f"Found: {self.report.version}"
            )
        else:
            self.report.add_result(
                "HDR_003",
                "Version number valid",
                CheckStatus.FAIL,
                "Expected semantic version (X.Y.Z)",
                "Header"
            )

        # Check status
        status_match = re.search(r'\*\*Status:\*\*\s*(\w+)', header_content)
        if status_match:
            status = status_match.group(1)
            self.report.status = status
            if status in self.VALID_STATUSES:
                self.report.add_result(
                    "HDR_004",
                    "Status value valid",
                    CheckStatus.PASS,
                    f"Found: {status}"
                )
            else:
                self.report.add_result(
                    "HDR_004",
                    "Status value valid",
                    CheckStatus.WARN,
                    f"Invalid status: {status}. Expected one of: {', '.join(self.VALID_STATUSES)}",
                    "Header"
                )
        else:
            self.report.add_result(
                "HDR_004",
                "Status value valid",
                CheckStatus.FAIL,
                "Status field not found in header",
                "Header"
            )

        # Check for required fields
        missing_fields = []
        for field in self.REQUIRED_HEADER_FIELDS:
            if field not in header_content:
                missing_fields.append(field)

        if missing_fields:
            self.report.add_result(
                "HDR_005",
                "All required header fields present",
                CheckStatus.FAIL,
                f"Missing fields: {', '.join(missing_fields)}",
                "Header"
            )
        else:
            self.report.add_result(
                "HDR_005",
                "All required header fields present",
                CheckStatus.PASS
            )

    def _validate_sections(self):
        """Validate required sections."""
        self.report.required_sections = self.REQUIRED_SECTIONS.copy()

        for i, section in enumerate(sorted(self.REQUIRED_SECTIONS)):
            # Look for section headers
            pattern = re.compile(r'^#+\s*' + re.escape(section), re.IGNORECASE)
            matches = self._find_line_numbers(pattern)

            if matches:
                self.report.sections_found.add(section)
                line_num = matches[0][0]
                self.report.add_result(
                    f"SEC_{i + 1:03d}",
                    f"Section present: {section}",
                    CheckStatus.PASS,
                    f"Found at line {line_num}",
                    f"Line {line_num}"
                )
            else:
                self.report.add_result(
                    f"SEC_{i + 1:03d}",
                    f"Section present: {section}",
                    CheckStatus.FAIL,
                    "Required section not found",
                    "N/A"
                )

    def _validate_roles(self):
        """Validate role definitions section."""
        start_line, end_line = self._get_section_lines("Role Definitions")

        if start_line == 0:
            # Role definitions section missing - already reported
            return

        section_content = '\n'.join(self.lines[start_line - 1:end_line])

        # Check for role table
        if "| Role ID" in section_content or "Role ID" in section_content:
            self.report.add_result(
                "ROL_001",
                "Role definitions table present",
                CheckStatus.PASS,
                "Found role table in Role Definitions section"
            )
        else:
            self.report.add_result(
                "ROL_001",
                "Role definitions table present",
                CheckStatus.WARN,
                "Role table not found - roles may be defined in free text"
            )

        # Check for role details subsections
        role_detail_pattern = re.compile(r'^#+\s+\*\*.*\*\*', re.MULTILINE)
        role_details = role_detail_pattern.findall(section_content)

        if len(role_details) >= 1:
            self.report.add_result(
                "ROL_002",
                "Role details subsections present",
                CheckStatus.PASS,
                f"Found {len(role_details)} role detail subsections"
            )
        else:
            self.report.add_result(
                "ROL_002",
                "Role details subsections present",
                CheckStatus.WARN,
                "No detailed role descriptions found"
            )

        # Check for required role detail fields
        required_fields = ["Description", "Type", "Responsibilities"]
        missing_fields = []

        for field in required_fields:
            field_pattern = re.compile(r'-\s*\*\*' + re.escape(field) + r':\*\*', re.IGNORECASE)
            if not field_pattern.search(section_content):
                missing_fields.append(field)

        if missing_fields:
            self.report.add_result(
                "ROL_003",
                "Role detail fields complete",
                CheckStatus.WARN,
                f"Missing fields in some roles: {', '.join(missing_fields)}"
            )
        else:
            self.report.add_result(
                "ROL_003",
                "Role detail fields complete",
                CheckStatus.PASS
            )

    def _validate_patterns(self):
        """Validate pattern context section."""
        start_line, end_line = self._get_section_lines("Pattern Context")

        if start_line == 0:
            return

        section_content = '\n'.join(self.lines[start_line - 1:end_line])

        # Find all pattern references
        pattern_matches = self.PATTERN_ID_RE.findall(section_content)
        patterns_found = {f"P{pid}" for pid in pattern_matches}

        if patterns_found:
            self.report.add_result(
                "PAT_001",
                "Pattern references found",
                CheckStatus.PASS,
                f"Found {len(patterns_found)} pattern(s): {', '.join(sorted(patterns_found)[:5])}"
                + ("..." if len(patterns_found) > 5 else "")
            )
        else:
            self.report.add_result(
                "PAT_001",
                "Pattern references found",
                CheckStatus.WARN,
                "No YAWL pattern IDs (P1-P43) found in Pattern Context"
            )

        # Check for pattern inventory table
        if "Pattern ID" in section_content and "Pattern Name" in section_content:
            self.report.add_result(
                "PAT_002",
                "Pattern inventory table present",
                CheckStatus.PASS
            )
        else:
            self.report.add_result(
                "PAT_002",
                "Pattern inventory table present",
                CheckStatus.WARN,
                "Pattern inventory table not found"
            )

    def _validate_traceability(self):
        """Validate traceability section."""
        start_line, end_line = self._get_section_lines("Traceability")

        if start_line == 0:
            return

        section_content = '\n'.join(self.lines[start_line - 1:end_line])

        # Check for requirements traceability
        if "Requirement" in section_content and "REQ_" in section_content:
            self.report.add_result(
                "TRC_001",
                "Requirements traceability present",
                CheckStatus.PASS
            )
        else:
            self.report.add_result(
                "TRC_001",
                "Requirements traceability present",
                CheckStatus.WARN,
                "No requirement identifiers (REQ_) found"
            )

        # Check for audit requirements
        if "Audit" in section_content or "audit" in section_content:
            self.report.add_result(
                "TRC_002",
                "Audit requirements documented",
                CheckStatus.PASS
            )
        else:
            self.report.add_result(
                "TRC_002",
                "Audit requirements documented",
                CheckStatus.WARN,
                "Audit requirements not mentioned"
            )

    def _validate_decision_schemas(self):
        """Validate decision schemas section."""
        start_line, end_line = self._get_section_lines("Decision Schemas")

        if start_line == 0:
            return

        section_content = '\n'.join(self.lines[start_line - 1:end_line])

        # Check for decision gates
        if "Decision Logic" in section_content or "decision" in section_content.lower():
            self.report.add_result(
                "DSC_001",
                "Decision logic documented",
                CheckStatus.PASS
            )
        else:
            self.report.add_result(
                "DSC_001",
                "Decision logic documented",
                CheckStatus.WARN,
                "No explicit decision logic found"
            )

        # Check for mermaid diagrams
        if "mermaid" in section_content:
            self.report.add_result(
                "DSC_002",
                "Decision diagrams present",
                CheckStatus.PASS
            )
        else:
            self.report.add_result(
                "DSC_002",
                "Decision diagrams present",
                CheckStatus.WARN,
                "No Mermaid diagrams found - consider adding visual decision flows"
            )

    def _validate_no_placeholders(self):
        """Check for incomplete placeholder text."""
        placeholders = ["TODO", "TBD", "TBC", "[TODO]", "[TBD]", "[TBC]",
                       "PLACEHOLDER", "<describe here>", "<add description>"]

        found_placeholders = []
        for i, line in enumerate(self.lines, start=1):
            for placeholder in placeholders:
                if placeholder in line.upper() and not line.strip().startswith("#"):
                    found_placeholders.append((i, placeholder, line.strip()[:50]))

        if found_placeholders:
            self.report.add_result(
                "QLT_001",
                "No placeholder text remaining",
                CheckStatus.WARN,
                f"Found {len(found_placeholders)} placeholder(s)"
            )
        else:
            self.report.add_result(
                "QLT_001",
                "No placeholder text remaining",
                CheckStatus.PASS
            )


def print_text_report(report: BCDValidationReport, verbose: bool = False):
    """Print validation report in human-readable text format.

    Args:
        report: The validation report to print
        verbose: Include detailed output for passing checks
    """
    print(f"\n{'=' * 70}")
    print(f"BCD Validation Report: {report.file_path}")
    print(f"{'=' * 70}\n")

    if report.document_id:
        print(f"Document ID: {report.document_id}")
    if report.version:
        print(f"Version: {report.version}")
    if report.status:
        print(f"Status: {report.status}")
    print()

    # Summary
    passed = sum(1 for r in report.results if r.status == CheckStatus.PASS)
    failed = sum(1 for r in report.results if r.status == CheckStatus.FAIL)
    warnings = sum(1 for r in report.results if r.status == CheckStatus.WARN)

    print(f"Summary: {passed} passed, {warnings} warnings, {failed} failed\n")

    # Failed checks first
    if failed > 0:
        print("FAILED CHECKS:")
        for result in report.results:
            if result.status == CheckStatus.FAIL:
                loc = f" [{result.location}]" if result.location else ""
                print(f"  [FAIL] {result.check_id}: {result.description}{loc}")
                if result.details:
                    print(f"        {result.details}")
        print()

    # Warnings
    if warnings > 0:
        print("WARNINGS:")
        for result in report.results:
            if result.status == CheckStatus.WARN:
                loc = f" [{result.location}]" if result.location else ""
                print(f"  [WARN] {result.check_id}: {result.description}{loc}")
                if result.details:
                    print(f"        {result.details}")
        print()

    # Verbose passing checks
    if verbose and passed > 0:
        print("PASSED CHECKS:")
        for result in report.results:
            if result.status == CheckStatus.PASS:
                loc = f" [{result.location}]" if result.location else ""
                print(f"  [PASS] {result.check_id}: {result.description}{loc}")
        print()

    # Overall result
    if failed == 0:
        print("Overall: VALIDATION PASSED")
        return 0
    else:
        print("Overall: VALIDATION FAILED")
        return 1


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Validate BCD (Business Context Document) structure"
    )
    parser.add_argument(
        "path",
        help="Path to BCD file or directory containing BCDs"
    )
    parser.add_argument(
        "--json", "-j",
        action="store_true",
        help="Output results in JSON format"
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

    # Find BCD files
    if path.is_dir():
        bcd_files = list(path.rglob("*.md"))
    else:
        bcd_files = [path]

    if not bcd_files:
        print("Error: No markdown files found", file=sys.stderr)
        return 2

    all_reports = []
    exit_code = 0

    for bcd_file in bcd_files:
        try:
            with open(bcd_file, 'r', encoding='utf-8') as f:
                content = f.read()

            validator = BCDValidator(content, str(bcd_file))
            report = validator.validate()
            all_reports.append(report)

            # Track failures for exit code
            if any(r.status == CheckStatus.FAIL for r in report.results):
                exit_code = 1

        except IOError as e:
            print(f"Error reading file {bcd_file}: {e}", file=sys.stderr)
            exit_code = 2

    # Generate output
    if args.json:
        if len(all_reports) == 1:
            output = json.dumps(all_reports[0].to_dict(), indent=2)
        else:
            output = json.dumps(
                {"reports": [r.to_dict() for r in all_reports]},
                indent=2
            )
    else:
        # Text output for single file, summary for multiple
        if len(all_reports) == 1:
            exit_code = print_text_report(all_reports[0], args.verbose)
        else:
            # Summary for multiple files
            print(f"\nBCD Validation: {len(all_reports)} files scanned\n")
            for report in all_reports:
                failed = sum(1 for r in report.results if r.status == CheckStatus.FAIL)
                warnings = sum(1 for r in report.results if r.status == CheckStatus.WARN)
                status = "PASS" if failed == 0 else "FAIL"
                print(f"  [{status}] {report.file_path}: "
                      f"{failed} failed, {warnings} warnings")
            print()

    # Write to file if requested
    if args.output:
        with open(args.output, 'w', encoding='utf-8') as f:
            f.write(output)
        print(f"\nOutput written to: {args.output}")
    else:
        if args.json:
            print(output)

    return exit_code


if __name__ == "__main__":
    sys.exit(main())
