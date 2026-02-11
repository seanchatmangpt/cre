# Cloud Ontology Linter - Quick Reference

## What's New

The `fibo_cloud_first_linter` now validates cloud deployment terms against **standard cloud ontology namespaces** and detects custom cloud terms.

## 3 New Functions

### 1. Validate Cloud Namespace
```erlang
% Check if namespace is standard (gcp, aws, azure, k8s, docker)
fibo_cloud_first_linter:validate_cloud_namespace(<<"gcp">>).
% Result: ok or {error, {unknown_cloud_ontology, Ns}}
```

### 2. Validate Cloud Term
```erlang
% Check if term is standard in its namespace
fibo_cloud_first_linter:validate_cloud_term(<<"gcp">>, <<"Deployment">>).
% Result: ok or {error, {custom_cloud_term, Ns, Term}}
```

### 3. Get Standard Terms
```erlang
% Retrieve all standard terms for a namespace
{ok, Terms} = fibo_cloud_first_linter:get_standard_cloud_terms(<<"k8s">>).
% Returns: [<<"Deployment">>, <<"Service">>, <<"Pod">>, ...]
```

## Cloud Namespaces Supported

| Namespace | Standard Terms | IRI |
|-----------|---|---|
| **gcp** | Deployment, Service, Instance, Pod, Cluster, Network, LoadBalancer, etc. | https://cloud.google.com/ontology/resources/ |
| **aws** | Instance, SecurityGroup, LoadBalancer, S3Bucket, IAMRole, etc. | https://aws.amazon.com/ontology/resources/ |
| **azure** | VirtualMachine, ResourceGroup, StorageAccount, KeyVault, etc. | https://ontology.microsoft.com/azure/resources/ |
| **k8s** | Deployment, Service, Pod, Node, Namespace, ConfigMap, etc. | https://kubernetes.io/ontology/api/ |
| **docker** | Container, Image, Network, Volume, Service, etc. | https://hub.docker.com/ontology/resources/ |

## Violation Types

### New: `custom_cloud_term_unjustified`
A cloud term is not in the standard vocabulary and lacks justification.

**Fix:** Either use a standard term or add `rdfs:comment` explaining the custom term.

```turtle
% ❌ WRONG - custom term without justification
gcp:CustomDeployment a owl:Class .

% ✅ RIGHT - standard term
gcp:Deployment a owl:Class .

% ✅ RIGHT - custom term WITH justification
gcp:CustomDeployment a owl:Class ;
    rdfs:comment "Custom extension for application-specific deployment patterns" .
```

## Linting Metrics

The lint result now includes:

```erlang
#{
    cloud_ontology_validated => 15,   % Standard cloud terms
    cloud_ontology_violations => 2,   % Custom/unknown terms
    violations => [
        #{
            type => custom_cloud_term_unjustified,
            term => <<"gcp:UnstandardTerm">>,
            namespace => <<"gcp">>,
            severity => medium
        }
    ]
}
```

## Usage Examples

### Lint a Deployment Ontology
```erlang
{ok, Result} = fibo_cloud_first_linter:lint_ontology("ontology/gke.ttl"),
Violations = maps:get(violations, Result),
io:format("Found ~p violations~n", [length(Violations)]).
```

### Check Single Term
```erlang
case fibo_cloud_first_linter:validate_cloud_term(<<"aws">>, <<"EC2Instance">>) of
    ok -> io:format("Standard AWS term~n");
    {error, E} -> io:format("Error: ~p~n", [E])
end.
```

### List All K8s Terms
```erlang
{ok, K8sTerms} = fibo_cloud_first_linter:get_standard_cloud_terms(<<"k8s">>),
lists:foreach(fun(T) -> io:format("- ~s~n", [T]) end, K8sTerms).
```

## TTL Ontology Best Practices

### ✅ Good Ontologies

```turtle
@prefix gcp: <https://cloud.google.com/ontology/resources/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

% Standard GCP term
gcp:Deployment a owl:Class ;
    rdfs:label "Cloud Deployment" .

% Custom term WITH justification
gcp:AppDeployment a owl:Class ;
    rdfs:label "Application-Specific Deployment" ;
    rdfs:comment "Extended from standard Deployment for Fortune-5 use cases" .
```

### ❌ Bad Ontologies

```turtle
% Custom term WITHOUT justification
gcp:WeirdResource a owl:Class ;
    rdfs:label "Some Resource" .
    % ← MISSING rdfs:comment explaining why custom

% Wrong namespace
mycloud:Deployment a owl:Class .  % ← Should be gcp:/aws:/etc.

% Typo in term
gpc:Deployment a owl:Class .  % ← 'gpc' is not a standard namespace
```

## Running Tests

```bash
# All cloud linter tests
rebar3 eunit --module=fibo_cloud_first_linter_test

# Specific test
rebar3 eunit --module=fibo_cloud_first_linter_test \
    --function=validate_standard_gcp_term_test

# Verbose
rebar3 eunit --module=fibo_cloud_first_linter_test --verbose
```

## Integration with CI/CD

### GitHub Actions Example
```yaml
- name: Validate Cloud Ontologies
  run: |
    rebar3 eunit --module=fibo_cloud_first_linter
    erlc -o /tmp ontology/*.ttl && \
    erl -noshell -s fibo_cloud_first_linter lint_ontology_dir \
        "ontology/" -s init stop
```

## Troubleshooting

| Problem | Solution |
|---------|----------|
| `unknown_cloud_ontology` error | Ensure namespace is one of: gcp, aws, azure, k8s, docker |
| `custom_cloud_term` violation | Add `rdfs:comment` or `skos:note` explaining the term |
| `invalid_fibo_iri` error | Check FIBO namespace (fibo-loan:, fibo-fnd:, etc.) |
| Term not found in standard list | It's a custom term - add documentation or use standard equivalent |

## Key Concepts

### Standard Terms vs Custom Terms
- **Standard**: Term appears in the ontology's standard vocabulary list
- **Custom**: Term does NOT appear in the standard vocabulary
- **Justified Custom**: Custom term with rdfs:comment explaining necessity
- **Unjustified Custom**: Custom term WITHOUT explanation → VIOLATION

### Namespaces
- **IRI**: Unique identifier for ontology (e.g., https://cloud.google.com/...)
- **Prefix**: Short name used in TTL (e.g., gcp:, aws:, k8s:)
- **Term**: Specific class/property (e.g., Deployment, Service, Pod)

## Performance Notes

- Ontology linting: O(n) where n = number of terms
- Term validation: O(1) lookup in standard terms list
- Directory linting: Parallel processing of .ttl files

## Standards Used

- **RDF/OWL**: W3C semantic web standards
- **FIBO**: Financial Industry Business Ontology
- **GCP**: Google Cloud Resource Model
- **AWS**: Amazon Web Services API ontology
- **Azure**: Microsoft Azure resource taxonomy
- **CNCF**: Kubernetes API standards

## For More Information

See `/docs/CLOUD_ONTOLOGY_VALIDATION.md` for complete documentation.
