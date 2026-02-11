# Cloud Ontology Namespace Validation

## Overview

The enhanced `fibo_cloud_first_linter` module validates that cloud deployment terms in ontologies conform to standard cloud vocabulary namespaces and use standard terminology. This ensures semantic interoperability with industry-standard cloud resource ontologies.

## Features

### 1. Standard Cloud Ontology Namespace Validation

Validates that cloud-related terms use recognized standard ontology namespaces:

- **GCP** (Google Cloud Platform)
  - IRI: `https://cloud.google.com/ontology/resources/`
  - Standard terms: Deployment, Service, Instance, Network, Firewall, LoadBalancer, StorageBucket, Database, AuthenticationPolicy, Cluster, Pod, ConfigMap, Secret, Namespace

- **AWS** (Amazon Web Services)
  - IRI: `https://aws.amazon.com/ontology/resources/`
  - Standard terms: Instance, SecurityGroup, LoadBalancer, AutoScalingGroup, S3Bucket, DynamoDBTable, IAMRole, EC2Instance, ElastiCacheCluster, RDSDatabase, CloudWatchMetric, CloudTrailEvent

- **Azure** (Microsoft Azure)
  - IRI: `https://ontology.microsoft.com/azure/resources/`
  - Standard terms: VirtualMachine, ResourceGroup, StorageAccount, CosmosDBDatabase, KeyVault, ApplicationGateway, VirtualNetwork, NetworkSecurityGroup, ManagedIdentity, AppService, AKSCluster, MonitoringAlert

- **K8s** (Kubernetes / CNCF)
  - IRI: `https://kubernetes.io/ontology/api/`
  - Standard terms: Deployment, Service, Pod, Node, ConfigMap, Secret, PersistentVolume, PersistentVolumeClaim, StatefulSet, DaemonSet, Namespace, Ingress, NetworkPolicy, Role, RoleBinding

- **Docker**
  - IRI: `https://hub.docker.com/ontology/resources/`
  - Standard terms: Container, Image, Registry, Network, Volume, Service, HealthCheck, BuildContext

### 2. Cloud Deployment Term Validation

Validates that cloud deployment terms conform to standard vocabularies for their namespace.

```erlang
%% Check if term is standard in its namespace
fibo_cloud_first_linter:validate_cloud_term(<<"gcp">>, <<"Deployment">>).
% Returns: ok

%% Custom term detection
fibo_cloud_first_linter:validate_cloud_term(<<"gcp">>, <<"CustomResource">>).
% Returns: {error, {custom_cloud_term, <<"gcp">>, <<"CustomResource">>}}
```

### 3. Standard Terminology Retrieval

Query standard terms for any supported cloud namespace.

```erlang
%% Get all standard GCP terms
{ok, Terms} = fibo_cloud_first_linter:get_standard_cloud_terms(<<"gcp">>).
% Returns terms like: [<<"Deployment">>, <<"Service">>, <<"Pod">>, ...]

%% Get AWS standard terms
{ok, AWSTerms} = fibo_cloud_first_linter:get_standard_cloud_terms(<<"aws">>).
```

### 4. Namespace Validation

Validate that a namespace is a recognized standard cloud ontology.

```erlang
%% Valid namespace
fibo_cloud_first_linter:validate_cloud_namespace(<<"gcp">>).
% Returns: ok

%% Invalid namespace
fibo_cloud_first_linter:validate_cloud_namespace(<<"custom_cloud">>).
% Returns: {error, {unknown_cloud_ontology, <<"custom_cloud">>}}
```

## Integration with Ontology Linting

The cloud ontology validation is integrated into the full ontology linting pipeline:

### Linting TTL Ontology Files

```erlang
{ok, Result} = fibo_cloud_first_linter:lint_ontology("ontology/deployment.ttl").
```

### Result Metrics

The linting result includes cloud ontology validation metrics:

```erlang
#{
    terms_checked => 50,
    fibo_aligned => 15,
    cloud_aligned => 20,
    cloud_ontology_validated => 18,    % Standard terms
    cloud_ontology_violations => 2,    % Custom/unknown terms
    custom_justified => 2,              % Custom terms with justification
    custom_unjustified => 0,
    violations => [
        #{
            type => custom_cloud_term_unjustified,
            term => <<"gcp:CustomDeploymentType">>,
            namespace => <<"gcp">>,
            severity => medium,
            suggestion => "Term not found in standard gcp ontology..."
        }
    ]
}
```

### Violation Types

1. **missing_cloud_alignment** (medium severity)
   - Cloud deployment term doesn't use standard cloud namespace
   - Suggestion: Use appropriate namespace (gcp:, aws:, azure:, k8s:, or docker:)

2. **invalid_cloud_namespace** (high severity)
   - Cloud term uses cloud namespace but namespace is invalid
   - Suggestion: Use valid namespace URL

3. **custom_cloud_term_unjustified** (medium severity)
   - Cloud term is not in standard vocabulary and lacks justification
   - Action: Either use standard term or add rdfs:comment/skos:note explaining custom term

4. **missing_fibo_alignment** (high severity)
   - Financial domain term doesn't use FIBO namespace
   - Suggestion: Use appropriate FIBO term

## Example TTL Ontology

```turtle
@prefix gcp: <https://cloud.google.com/ontology/resources/> .
@prefix k8s: <https://kubernetes.io/ontology/api/> .
@prefix fibo-loan: <https://spec.edmcouncil.org/fibo/ontology/LOAN/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

% VALID: Standard GCP deployment term
gcp:Deployment a owl:Class ;
    rdfs:label "Google Cloud Deployment" ;
    rdfs:comment "Standard deployment resource in GCP" .

% VALID: Standard Kubernetes term
k8s:Pod a owl:Class ;
    rdfs:label "Kubernetes Pod" ;
    rdfs:comment "Smallest deployable unit in Kubernetes" .

% VALID: Custom term WITH justification
gcp:CustomDeploymentType a owl:Class ;
    rdfs:label "Custom Deployment Type" ;
    rdfs:comment "Custom term extending GCP for application-specific deployment patterns used in Fortune-5 systems" .

% INVALID: Custom term WITHOUT justification (will generate violation)
gcp:UnstandardResource a owl:Class ;
    rdfs:label "Non-standard Resource" .

% VALID: FIBO-aligned financial term
fibo-loan:Loan a owl:Class ;
    rdfs:label "Loan Contract" .
```

## Usage Examples

### Validate Single Ontology File

```erlang
{ok, Result} = fibo_cloud_first_linter:lint_ontology("ontology/cloud_deployment.ttl"),
Proof = fibo_cloud_first_linter:generate_proof(Result),
io:format("Cloud ontology validation: ~p~n", [maps:get(compliant, Proof)]).
```

### Validate Ontology Directory

```erlang
{ok, Results} = fibo_cloud_first_linter:lint_ontology_dir("ontology/"),
lists:foreach(fun(R) ->
    CloudViolations = maps:get(cloud_ontology_violations, R, 0),
    io:format("Cloud violations: ~p~n", [CloudViolations])
end, Results).
```

### Check Specific Cloud Term

```erlang
case fibo_cloud_first_linter:validate_cloud_term(<<"aws">>, <<"EC2Instance">>) of
    ok ->
        io:format("Term is standard AWS resource~n");
    {error, {custom_cloud_term, NS, Term}} ->
        io:format("Custom term: ~s:~s - needs justification~n", [NS, Term])
end.
```

### Generate Compliance Proof

```erlang
{ok, Result} = fibo_cloud_first_linter:lint_ontology("ontology/app.ttl"),
Proof = fibo_cloud_first_linter:generate_proof(Result),
io:format("Compliance Hash: ~s~n", [maps:get(hash, Proof)]).
```

## API Reference

### Core Functions

#### `lint_ontology(File) -> {ok, lint_result()} | {error, term()}`
Lint a single TTL ontology file for FIBO and cloud vocabulary compliance.

**Parameters:**
- `File` (string): Path to TTL file

**Returns:**
- `ok` with lint_result map containing validation metrics
- `error` with reason

**Result Map Keys:**
- `terms_checked`: Total terms analyzed
- `fibo_aligned`: Terms using FIBO namespaces
- `cloud_aligned`: Terms using cloud namespaces
- `cloud_ontology_validated`: Standard cloud terms
- `cloud_ontology_violations`: Custom cloud terms without justification
- `violations`: List of violation details

#### `validate_cloud_term(Namespace, Term) -> ok | {error, term()}`
Check if a term is a standard term in a cloud namespace.

**Parameters:**
- `Namespace` (binary): Cloud namespace (gcp, aws, azure, k8s, docker)
- `Term` (binary): Term name to validate

**Returns:**
- `ok` if term is standard
- `{error, {custom_cloud_term, NS, Term}}` if custom
- `{error, {unknown_cloud_ontology, NS}}` if invalid namespace

#### `validate_cloud_namespace(Namespace) -> ok | {error, term()}`
Check if namespace is a recognized standard cloud ontology.

**Parameters:**
- `Namespace` (binary): Namespace identifier

**Returns:**
- `ok` if valid standard namespace
- `{error, {unknown_cloud_ontology, NS}}` if not recognized

#### `get_standard_cloud_terms(Namespace) -> {ok, [binary()]} | {error, term()}`
Retrieve list of standard terms for a cloud namespace.

**Parameters:**
- `Namespace` (binary): Cloud namespace

**Returns:**
- `{ok, Terms}` list of standard term names
- `{error, {unknown_cloud_ontology, NS}}` if invalid namespace

#### `generate_proof(LintResult) -> map()`
Generate compliance proof with hash for audit trail.

**Parameters:**
- `LintResult` (map): Result from lint_ontology/1

**Returns:** Map with:
- `proof_type`: "FIBO_Cloud_First_Compliance"
- `compliant`: Boolean
- `violations_count`: Number of violations
- `hash`: SHA256 hash of normalized result

## Testing

Comprehensive test suite is available in `test/fibo_cloud_first_linter_test.erl`:

```bash
# Run all linter tests
rebar3 eunit --module=fibo_cloud_first_linter_test

# Run specific test group
rebar3 eunit --module=fibo_cloud_first_linter_test \
    --function=gcp_namespace_validation_test

# Run with verbose output
rebar3 eunit --module=fibo_cloud_first_linter_test --verbose
```

## Best Practices

### 1. Use Standard Namespaces
Prefer standard cloud namespaces (gcp:, aws:, azure:, k8s:) over custom cloud namespaces.

### 2. Document Custom Terms
If custom cloud terms are necessary, always include `rdfs:comment` or `skos:note` explaining the need.

```turtle
gcp:CustomWorkflowResource a owl:Class ;
    rdfs:comment "Custom extension for Fortune-5 workflow orchestration not covered by standard GCP ontology" .
```

### 3. Align Financial Terms with FIBO
Always use FIBO vocabulary for financial domain terms.

```turtle
fibo-loan:Loan a owl:Class .      % Correct
custom:Loan a owl:Class .         % Incorrect
```

### 4. Validate Early
Run linting in CI/CD pipeline to catch compliance issues before deployment.

## Cloud Vocabulary Resources

- **GCP**: https://cloud.google.com/docs/reference/rest
- **AWS**: https://docs.aws.amazon.com/
- **Azure**: https://learn.microsoft.com/en-us/azure/
- **Kubernetes**: https://kubernetes.io/docs/
- **FIBO**: https://spec.edmcouncil.org/fibo/

## Troubleshooting

### "Unknown Cloud Ontology" Error
Ensure namespace is one of: gcp, aws, azure, k8s, docker

### Custom Term Warnings
Add documentation to custom cloud terms:
```turtle
yourns:CustomTerm a owl:Class ;
    rdfs:comment "Reason for custom term..." .
```

### IRI Validation Failures
Ensure FIBO terms use correct namespace prefixes:
- fibo-loan: for loan concepts
- fibo-fnd: for foundation concepts
- fibo-be: for business entities

## Future Enhancements

- HTTP validation of cloud ontology IRIs
- Semantic similarity matching for custom terms
- Integration with W3C SHACL for declarative validation
- Export validation results to SPDX/CycloneDX SBOM formats
