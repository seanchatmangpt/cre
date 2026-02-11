# SPARQL Extraction Queries

These queries extract from the RDF ontology to drive code generation.

## Queries

- `incidents.rq` - Extract incident classifications, severity levels, lifecycle states
- `gates.rq` - Extract decision gates, conditions, true/false paths
- `connectors.rq` - Extract connector specs, operations, effect types
- `lines.rq` - Extract line definitions, sequences of gates and stations

**Input**: Ontology pack from `/home/user/cre/ontology/security-ops/`
**Output**: RDF results used by ggen to generate code

## Usage

```
ggen sparql -i ontology/security-ops/ -q sparql/incidents.rq -o output/incidents.rdf
```
