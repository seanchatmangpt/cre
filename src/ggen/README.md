# ggen - Manufacturing Pipeline

RDF ontology → SPARQL extraction → Tera templates → generated Erlang + infrastructure.

## Modules

- `ggen.erl` - Main entry point (CLI + orchestration)
- `ggen_rdf.erl` - RDF loader + validator
- `ggen_sparql.erl` - SPARQL query executor
- `ggen_template.erl` - Tera template renderer
- `ggen_codegen.erl` - Erlang AST generation (fallback)
- `ggen_rules.erl` - Generation rules engine
- `ggen_validate.erl` - Quality gates (schema, closure, determinism checks)
- `ggen_andon.erl` - Status signaling (green/yellow/red)

## CLI Commands

```
ggen validate <ontology_dir>      # Check ontology integrity
ggen extract <ontology_dir>       # Run SPARQL queries
ggen generate <context.json>      # Run template generation
ggen sync                         # Full pipeline: validate → extract → generate
ggen show_andon                   # Display status (green/yellow/red)
```

## Usage Example

```
ggen validate ontology/security-ops/
ggen sync --ontology ontology/security-ops/ --output generated/
```

Output:
- `generated/soc_triage_line.erl`
- `generated/soc_triage_app.erl`
- `generated/incident_connector_*.erl` (4 connectors)
- `generated/Dockerfile`
- `generated/terraform/main.tf`
