#!/usr/bin/env python3
"""
Generate customer-specific SOC 2 compliance artifacts.

For each customer with requiresSuite in ontology/customers.ttl, generates:
- lib/soc2/customers/{customer_id}.soc2.control_matrix.yaml
- lib/soc2/customers/{customer_id}.soc2.auditor_pack.json

Uses SPARQL queries against the SOC 2 and Customer ontologies.
"""

import json
import yaml
import sys
from pathlib import Path
from typing import List, Dict, Any, Set, Tuple
from dataclasses import dataclass
from urllib.parse import urlparse
import rdflib
from rdflib import Graph, Namespace, URIRef
from rdflib.term import Literal

# RDF/SPARQL Query support
from rdflib.plugins.sparql import prepareQuery


@dataclass
class Control:
    """Represents a SOC 2 control."""
    control_id: str
    title: str
    description: str
    tsc_id: str
    tsc_label: str
    validators: List[str]
    evidence: List[Dict[str, Any]]


@dataclass
class Suite:
    """Represents a SOC 2 suite."""
    suite_id: str
    suite_name: str
    categories: Dict[str, Tuple[str, List[Control]]]  # tsc_id -> (tsc_label, [controls])


@dataclass
class Customer:
    """Represents a customer with SOC 2 requirements."""
    customer_id: str
    customer_name: str
    suite_id: str
    suite_name: str


class SOC2ArtifactGenerator:
    """Generate customer-specific SOC 2 compliance artifacts."""

    def __init__(self, project_dir: Path):
        self.project_dir = project_dir
        self.graph = Graph()

        # Namespace definitions
        self.REG = Namespace("http://fortune5.lnctrl.io/regulation#")
        self.CUST = Namespace("http://fortune5.lnctrl.io/customers#")
        self.RDFS = Namespace("http://www.w3.org/2000/01/rdf-schema#")
        self.RDF = Namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
        self.OWL = Namespace("http://www.w3.org/2002/07/owl#")
        self.XSD = Namespace("http://www.w3.org/2001/XMLSchema#")

    def load_ontologies(self) -> None:
        """Load SOC 2 and Customer ontologies from TTL files."""
        soc2_file = self.project_dir / "ontology" / "reg" / "soc2.ttl"
        customers_file = self.project_dir / "ontology" / "customers.ttl"

        print(f"Loading SOC 2 ontology from {soc2_file}")
        if not soc2_file.exists():
            raise FileNotFoundError(f"SOC 2 ontology not found: {soc2_file}")
        self.graph.parse(str(soc2_file), format="turtle")

        print(f"Loading Customer ontology from {customers_file}")
        if not customers_file.exists():
            raise FileNotFoundError(f"Customer ontology not found: {customers_file}")
        self.graph.parse(str(customers_file), format="turtle")

    def extract_customers_with_suites(self) -> List[Customer]:
        """Extract all customers that have requiresSuite specified."""
        customers = []

        # Note: The SOC 2 ontology uses "reg:" prefix for "http://fortune5.lnctrl.io/regulation#"
        # while the SPARQL queries in ggen.toml and extract_customer_control_closure.sparql
        # use the same namespace, so we need to query for the actual URIs
        query_str = """
        PREFIX reg: <http://fortune5.lnctrl.io/regulation#>
        PREFIX cust: <http://fortune5.lnctrl.io/customers#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

        SELECT DISTINCT ?customerId ?customerName ?suiteId ?suiteName
        WHERE {
            ?customer a cust:Customer ;
                      cust:customerId ?customerId ;
                      cust:customerName ?customerName ;
                      cust:requiresSuite ?suite .

            ?suite a reg:Suite ;
                   rdfs:label ?suiteName .

            BIND(STRAFTER(STR(?suite), "#") AS ?suiteId)
        }
        ORDER BY ?customerId
        """

        query = prepareQuery(query_str)
        results = self.graph.query(query)

        for row in results:
            customer = Customer(
                customer_id=str(row.customerId),
                customer_name=str(row.customerName),
                suite_id=str(row.suiteId),
                suite_name=str(row.suiteName)
            )
            customers.append(customer)
            print(f"  Found customer: {customer.customer_id} ({customer.customer_name}) "
                  f"-> {customer.suite_id}")

        return customers

    def extract_suite_controls(self, suite_id: str) -> Tuple[str, Dict[str, Tuple[str, List[Dict[str, Any]]]]]:
        """
        Extract all controls for a given suite.
        Returns: (suite_name, {tsc_id -> (tsc_label, [control_dicts])})
        """
        query_str = """
        PREFIX reg: <http://fortune5.lnctrl.io/regulation#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

        SELECT DISTINCT
            ?suiteName
            ?tscLabel
            ?tscId
            ?controlId
            ?controlTitle
            ?controlDescription
            ?validatorId
            ?evidenceId
            ?evidencePath
            ?evidenceFormat
            ?evidenceIntegrity
            ?retentionDays
        WHERE {
            ?suite a reg:Suite ;
                   rdfs:label ?suiteName .

            BIND(STRAFTER(STR(?suite), "#") AS ?suiteId)
            FILTER(?suiteId = "%s")

            ?suite reg:includesCategory ?tsc .
            ?tsc rdfs:label ?tscLabel .
            BIND(STRAFTER(STR(?tsc), "#") AS ?tscId)

            ?control a reg:Control ;
                     reg:tscCategory ?tsc ;
                     reg:controlId ?controlId ;
                     reg:title ?controlTitle ;
                     reg:description ?controlDescription .

            ?control reg:requiresValidator ?validatorId .

            ?control reg:requiresEvidence ?evidence .
            ?evidence reg:evidenceId ?evidenceId ;
                      reg:path ?evidencePath ;
                      reg:format ?evidenceFormat ;
                      reg:integrity ?evidenceIntegrity .

            OPTIONAL {
                ?evidence reg:retentionDays ?retentionDays .
            }
        }
        ORDER BY ?tscId ?controlId ?validatorId ?evidencePath
        """ % suite_id

        query = prepareQuery(query_str)
        results = self.graph.query(query)

        suite_name = None
        categories = {}

        for row in results:
            suite_name = str(row.suiteName)
            tsc_id = str(row.tscId)
            tsc_label = str(row.tscLabel)
            control_id = str(row.controlId)
            control_title = str(row.controlTitle)
            control_description = str(row.controlDescription)
            validator_id = str(row.validatorId)
            evidence_id = str(row.evidenceId)
            evidence_path = str(row.evidencePath)
            evidence_format = str(row.evidenceFormat)
            evidence_integrity = str(row.evidenceIntegrity)
            retention_days = int(row.retentionDays) if row.retentionDays else None

            # Initialize category if not present
            if tsc_id not in categories:
                categories[tsc_id] = (tsc_label, {})

            _, controls_dict = categories[tsc_id]

            # Initialize control if not present
            if control_id not in controls_dict:
                controls_dict[control_id] = {
                    "control_id": control_id,
                    "title": control_title,
                    "description": control_description,
                    "validators": [],
                    "evidence": []
                }

            control_dict = controls_dict[control_id]

            # Add validator if not already present
            if validator_id not in control_dict["validators"]:
                control_dict["validators"].append(validator_id)

            # Add evidence if not already present
            evidence_entry = {
                "evidence_id": evidence_id,
                "path": evidence_path,
                "format": evidence_format,
                "integrity": evidence_integrity
            }
            if retention_days is not None:
                evidence_entry["retention_days"] = retention_days

            # Check if this evidence already exists
            evidence_exists = any(
                e["evidence_id"] == evidence_id for e in control_dict["evidence"]
            )
            if not evidence_exists:
                control_dict["evidence"].append(evidence_entry)

        # Convert control dicts to lists
        formatted_categories = {}
        for tsc_id, (tsc_label, controls_dict) in categories.items():
            controls_list = list(controls_dict.values())
            formatted_categories[tsc_id] = (tsc_label, controls_list)

        return suite_name, formatted_categories

    def generate_control_matrix_yaml(
        self,
        customer: Customer,
        categories: Dict[str, Tuple[str, List[Dict[str, Any]]]]
    ) -> str:
        """Generate YAML control matrix for a customer."""
        matrix = {
            "control_matrix": {
                "metadata": {
                    "customer_id": customer.customer_id,
                    "customer_name": customer.customer_name,
                    "suite_id": customer.suite_id,
                    "suite_name": customer.suite_name,
                    "generated_by": "generate_customer_soc2_artifacts.py",
                    "version": "1.0.0"
                },
                "suites": [
                    {
                        "suite_id": customer.suite_id,
                        "suite_name": customer.suite_name,
                        "categories": [
                            {
                                "category_id": tsc_id,
                                "category_name": tsc_label,
                                "controls": controls
                            }
                            for tsc_id, (tsc_label, controls) in sorted(categories.items())
                        ]
                    }
                ]
            }
        }

        return yaml.dump(matrix, default_flow_style=False, sort_keys=False)

    def generate_auditor_pack_json(
        self,
        customer: Customer,
        categories: Dict[str, Tuple[str, List[Dict[str, Any]]]]
    ) -> str:
        """Generate JSON auditor pack for a customer."""
        # Collect all unique validators
        validators = set()
        # Collect all unique evidence
        evidence_map = {}
        # Collect control-evidence-validator mappings
        control_mappings = []

        for tsc_id, (tsc_label, controls) in categories.items():
            for control in controls:
                for validator_id in control["validators"]:
                    validators.add(validator_id)

                for evidence in control["evidence"]:
                    evidence_id = evidence["evidence_id"]
                    evidence_map[evidence_id] = evidence

                    for validator_id in control["validators"]:
                        control_mappings.append({
                            "control_id": control["control_id"],
                            "validator": validator_id,
                            "evidence": evidence_id
                        })

        auditor_pack = {
            "soc2_auditor_pack": {
                "version": "1.0.0",
                "generated_by": "generate_customer_soc2_artifacts.py",
                "customer_id": customer.customer_id,
                "customer_name": customer.customer_name,
                "suites_evaluated": [
                    {
                        "suite_id": customer.suite_id,
                        "suite_name": customer.suite_name
                    }
                ],
                "evidence_manifest": {
                    "manifest_file": "evidence/evidence.sha256",
                    "integrity_mechanism": "sha256_manifest",
                    "required_evidence": sorted(
                        evidence_map.values(),
                        key=lambda x: x["path"]
                    )
                },
                "validator_registry": sorted(list(validators)),
                "control_evidence_mapping": sorted(
                    control_mappings,
                    key=lambda x: (x["control_id"], x["validator"], x["evidence"])
                ),
                "receipt_fields": {
                    "build_receipt": "receipts/build.last.json",
                    "evidence_manifest": "receipts/evidence.last.json",
                    "verdict": "receipts/verdict.last.json"
                },
                "notes": [
                    f"Customer-specific SOC 2 auditor pack for {customer.customer_name}",
                    f"Audit suite: {customer.suite_name}",
                    "All evidence files are indexed in evidence/evidence.sha256",
                    "Receipt files contain cryptographic proof hashes",
                    "Generated deterministically - same ontology produces identical pack"
                ]
            }
        }

        return json.dumps(auditor_pack, indent=2)

    def generate_all_artifacts(self) -> None:
        """Generate all customer-specific SOC 2 artifacts."""
        print("\n" + "="*80)
        print("SOC 2 Customer-Specific Artifact Generator")
        print("="*80)

        # Load ontologies
        print("\nLoading ontologies...")
        self.load_ontologies()

        # Extract customers
        print("\nExtracting customers with requiresSuite...")
        customers = self.extract_customers_with_suites()

        if not customers:
            print("ERROR: No customers with requiresSuite found in ontology")
            return

        print(f"\nFound {len(customers)} customers with SOC 2 suite requirements")

        # Create output directory
        output_dir = self.project_dir / "lib" / "soc2" / "customers"
        output_dir.mkdir(parents=True, exist_ok=True)
        print(f"\nOutput directory: {output_dir}")

        # Generate artifacts for each customer
        print("\nGenerating customer-specific artifacts...\n")
        for i, customer in enumerate(customers, 1):
            print(f"[{i}/{len(customers)}] {customer.customer_id}")

            # Extract controls for this customer's suite
            suite_name, categories = self.extract_suite_controls(customer.suite_id)

            # Generate control matrix YAML
            yaml_content = self.generate_control_matrix_yaml(customer, categories)
            yaml_file = output_dir / f"{customer.customer_id}.soc2.control_matrix.yaml"
            yaml_file.write_text(yaml_content)
            print(f"  ✓ Generated {yaml_file.name}")

            # Generate auditor pack JSON
            json_content = self.generate_auditor_pack_json(customer, categories)
            json_file = output_dir / f"{customer.customer_id}.soc2.auditor_pack.json"
            json_file.write_text(json_content)
            print(f"  ✓ Generated {json_file.name}")

        print("\n" + "="*80)
        print(f"SUCCESS: Generated artifacts for {len(customers)} customers")
        print(f"Location: {output_dir}")
        print("="*80 + "\n")


def main():
    """Entry point."""
    if len(sys.argv) > 1:
        project_dir = Path(sys.argv[1])
    else:
        project_dir = Path(__file__).parent.parent

    try:
        generator = SOC2ArtifactGenerator(project_dir)
        generator.generate_all_artifacts()
    except Exception as e:
        print(f"ERROR: {e}", file=sys.stderr)
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
