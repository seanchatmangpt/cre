# YAWL Specification Handling Analysis

## Executive Summary

This document analyzes the YAWL (Yet Another Workflow Language) Java specification handling and compares it with CRE's YAML 0.2 format. The analysis covers the XML schema, loading process, validation mechanisms, and design trade-offs between the two approaches.

**Key Findings:**
- YAWL uses a verbose XML-based schema with XSD validation (Beta 3 through 4.0)
- CRE uses a concise YAML 0.2 format with pattern-first design
- YAWL supports multi-instance tasks, resourcing, timers, and web service gateways
- CRE extends YAWL with pattern instances, pattern registry, and usage indexing

---

## Table of Contents

1. [YAWL XML Schema Structure](#1-yawl-xml-schema-structure)
2. [Specification Loading Process](#2-specification-loading-process)
3. [Specification Validation](#3-specification-validation)
4. [YAWL Java Class Hierarchy](#4-yawl-java-class-hierarchy)
5. [CRE YAML 0.2 Format](#5-cre-yaml-02-format)
6. [Schema Comparison](#6-schema-comparison)
7. [Examples Side-by-Side](#7-examples-side-by-side-side)
8. [Feature Mapping](#8-feature-mapping)
9. [Recommendations](#9-recommendations)

---

## 1. YAWL XML Schema Structure

### 1.1 Root Element: `specificationSet`

```xml
<specificationSet xmlns="http://www.yawlfoundation.org/yawlschema"
                  version="4.0">
    <specification uri="...">
        <!-- specification content -->
    </specification>
    <layout>...</layout>  <!-- Optional, stripped during loading -->
</specificationSet>
```

**Attributes:**
- `version`: Schema version (Beta 3, Beta 4, Beta 6, Beta 7.1, 2.0, 2.1, 2.2, 3.0, 4.0)
- `xmlns`: YAWL namespace

### 1.2 Specification Element Structure

```xml
<specification uri="MakeRecordings">
    <name>Cut Records</name>
    <documentation>Description here...</documentation>
    <metaData>
        <title>...</title>
        <creator>...</creator>
        <version>...</version>
        <identifier>...</identifier>
        <!-- Dublin Core metadata -->
    </metaData>

    <!-- Data Schema (XSD types) -->
    <xs:schema>...</xs:schema>

    <!-- Decompositions (Nets and WebServiceGateways) -->
    <decomposition id="NetID" isRootNet="true" xsi:type="NetFactsType">
        <!-- Net content -->
    </decomposition>
    <decomposition id="ServiceID" xsi:type="WebServiceGatewayFactsType">
        <!-- Web service content -->
    </decomposition>
</specification>
```

### 1.3 Schema Versions History

| Version | Key Features | XSD Location |
|---------|-------------|--------------|
| Beta 3 | Initial schema format | `YAWL_SchemaBeta3.xsd` |
| Beta 4 | Added `isRootNet` attribute | `YAWL_SchemaBeta4.xsd` |
| Beta 6 | Enhanced metadata | `YAWL_SchemaBeta6.xsd` |
| Beta 7.1 | Resourcing improvements | `YAWL_SchemaBeta7.1.xsd` |
| 2.0 | Major refactor | `YAWL_Schema2.0.xsd` |
| 2.1 | External data gateways | `YAWL_Schema2.1.xsd` |
| 2.2 | Configuration support | `YAWL_Schema2.2.xsd` |
| 3.0 | Layout schema | `YAWL_Schema3.0.xsd` |
| 4.0 | Current stable | `YAWL_Schema4.0.xsd` |

### 1.4 Core Schema Types

#### 1.4.1 Control Types

```xml
<!-- Simple types for split/join behavior -->
<xs:simpleType name="ControlTypeCodeType">
    <xs:restriction base="xs:string">
        <xs:enumeration value="and"/>
        <xs:enumeration value="or"/>
        <xs:enumeration value="xor"/>
    </xs:restriction>
</xs:simpleType>
```

#### 1.4.2 Net Element Types

```xml
<xs:complexType name="ExternalNetElementFactsType">
    <xs:sequence>
        <xs:element name="name" type="yawl:LabelType" minOccurs="0"/>
        <xs:element name="documentation" type="yawl:DocumentationType" minOccurs="0"/>
        <xs:element name="flowsInto" type="yawl:FlowsIntoType" maxOccurs="unbounded"/>
    </xs:sequence>
    <xs:attribute name="id" type="yawl:NetElementIDType" use="required"/>
</xs:complexType>
```

#### 1.4.3 Task Types

```xml
<xs:complexType name="ExternalTaskFactsType">
    <xs:complexContent>
        <xs:extension base="yawl:ExternalNetElementFactsType">
            <xs:sequence>
                <xs:element name="join" type="yawl:ControlTypeType"/>
                <xs:element name="split" type="yawl:ControlTypeType"/>
                <xs:element name="removesTokens" type="yawl:ExternalNetElementType"
                            minOccurs="0" maxOccurs="unbounded"/>
                <xs:element name="startingMappings" type="yawl:VarMappingSetType"
                            minOccurs="0"/>
                <xs:element name="completedMappings" type="yawl:VarMappingSetType"
                            minOccurs="0"/>
                <xs:element name="enablementMappings" type="yawl:VarMappingSetType"
                            minOccurs="0"/>
                <xs:element name="timer" type="yawl:TimerType" minOccurs="0"/>
                <xs:element name="resourcing" type="yawl:ResourcingFactsType"
                            minOccurs="0"/>
                <xs:element name="decomposesTo" type="yawl:DecompositionType"
                            minOccurs="0"/>
            </xs:sequence>
        </xs:extension>
    </xs:complexContent>
</xs:complexType>
```

#### 1.4.4 Multi-Instance Task Type

```xml
<xs:complexType name="MultipleInstanceExternalTaskFactsType">
    <xs:complexContent>
        <xs:extension base="yawl:ExternalTaskFactsType">
            <xs:sequence>
                <xs:element name="minimum" type="yawl:XQueryType"/>
                <xs:element name="maximum" type="yawl:XQueryType"/>
                <xs:element name="threshold" type="yawl:XQueryType"/>
                <xs:element name="creationMode" type="yawl:CreationModeType"/>
                <xs:element name="miDataInput">...</xs:element>
                <xs:element name="miDataOutput" minOccurs="0">...</xs:element>
            </xs:sequence>
        </xs:extension>
    </xs:complexContent>
</xs:complexType>
```

### 1.5 Net Structure (NetFactsType)

```xml
<xs:complexType name="NetFactsType">
    <xs:complexContent>
        <xs:extension base="yawl:DecompositionFactsType">
            <xs:sequence>
                <xs:element name="localVariable" type="yawl:VariableFactsType"
                            minOccurs="0" maxOccurs="unbounded"/>
                <xs:element name="processControlElements">
                    <xs:complexType>
                        <xs:sequence>
                            <xs:element name="inputCondition"
                                        type="yawl:ExternalConditionFactsType"/>
                            <xs:choice maxOccurs="unbounded">
                                <xs:element name="task"
                                            type="yawl:ExternalTaskFactsType"/>
                                <xs:element name="condition"
                                            type="yawl:ExternalConditionFactsType"/>
                            </xs:choice>
                            <xs:element name="outputCondition"
                                        type="yawl:OutputConditionFactsType"/>
                        </xs:sequence>
                    </xs:complexType>
                </xs:element>
                <xs:element name="externalDataGateway" type="yawl:NameType"
                            minOccurs="0"/>
            </xs:sequence>
            <xs:attribute name="isRootNet" type="xs:boolean"/>
        </xs:extension>
    </xs:complexContent>
</xs:complexType>
```

### 1.6 WebService Gateway Type

```xml
<xs:complexType name="WebServiceGatewayFactsType">
    <xs:complexContent>
        <xs:extension base="yawl:DecompositionFactsType">
            <xs:sequence>
                <xs:element name="enablementParam" type="yawl:InputParameterFactsType"
                            minOccurs="0" maxOccurs="unbounded"/>
                <xs:element name="yawlService" minOccurs="0">
                    <xs:complexType>
                        <xs:sequence>
                            <xs:element name="documentation" type="xs:string"
                                        minOccurs="0"/>
                            <xs:choice>
                                <xs:sequence>
                                    <xs:element name="wsdlLocation" type="xs:anyURI"/>
                                    <xs:element name="operationName" type="xs:NMTOKEN"/>
                                </xs:sequence>
                                <xs:sequence/>
                            </xs:choice>
                        </xs:sequence>
                        <xs:attribute name="id" type="yawl:YAWLServiceIDType"
                                      use="required"/>
                    </xs:complexType>
                </xs:element>
                <xs:element name="codelet" type="xs:NCName" minOccurs="0"/>
                <xs:element name="externalInteraction"
                            type="yawl:ResourcingExternalInteractionType"
                            minOccurs="0"/>
            </xs:sequence>
            <xs:anyAttribute processContents="skip"/>
        </xs:extension>
    </xs:complexContent>
</xs:complexType>
```

---

## 2. Specification Loading Process

### 2.1 Loading Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         YAWL Specification Loading                       │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  XML String                                                             │
│     │                                                                   │
│     ▼                                                                   │
│  ┌─────────────────┐                                                    │
│  │ JDOMUtil        │  Parse XML to JDOM Document                        │
│  │ stringToDocument│                                                    │
│  └────────┬────────┘                                                    │
│           │                                                            │
│           ▼                                                            │
│  ┌─────────────────┐                                                    │
│  │ getVersion()    │  Extract version from root attribute               │
│  └────────┬────────┘                                                    │
│           │                                                            │
│           ▼                                                            │
│  ┌─────────────────┐                                                    │
│  │ SchemaHandler   │  Validate against XSD (optional)                   │
│  │ compileAndValid│                                                    │
│  └────────┬────────┘                                                    │
│           │                                                            │
│           ▼                                                            │
│  ┌─────────────────┐                                                    │
│  │ strip layout    │  Remove layout elements (not used by engine)       │
│  └────────┬────────┘                                                    │
│           │                                                            │
│           ▼                                                            │
│  ┌─────────────────────────────────────────────────────────┐            │
│  │ YSpecificationParser                                     │            │
│  │   ├─ parse specification element                         │            │
│  │   ├─ parse metadata                                      │            │
│  │   ├─ parse schema (data types)                           │            │
│  │   ├─ parse decompositions (nets & services)              │            │
│  │   └─ create YSpecification object                         │            │
│  └────────┬─────────────────────────────────────────────────┘            │
│           │                                                            │
│           ▼                                                            │
│  ┌─────────────────┐                                                    │
│  │ buildSpecs()    │  Build list of YSpecification objects              │
│  └────────┬────────┘                                                    │
│           │                                                            │
│           ▼                                                            │
│  List<YSpecification>                                                   │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

### 2.2 YMarshal Class (`YMarshal.java`)

```java
public class YMarshal {
    public static List<YSpecification> unmarshalSpecifications(String specStr)
            throws YSyntaxException {
        return unmarshalSpecifications(specStr, true);
    }

    public static List<YSpecification> unmarshalSpecifications(
            String specStr, boolean schemaValidate) throws YSyntaxException {

        // Parse XML string to JDOM Document
        Document document = JDOMUtil.stringToDocument(specStr);
        Element specificationSetEl = document.getRootElement();

        // Determine schema version
        YSchemaVersion version = getVersion(specificationSetEl);
        Namespace ns = specificationSetEl.getNamespace();

        // Strip layout element (engine doesn't use it)
        specificationSetEl.removeChild("layout", ns);

        // Schema validation (if enabled)
        if (schemaValidate) {
            SchemaHandler validator = new SchemaHandler(version.getSchemaURL());
            if (!validator.compileAndValidate(specStr)) {
                throw new YSyntaxException(
                    "The specification file failed to verify against YAWL's Schema:\n"
                    + validator.getConcatenatedMessage());
            }
        }

        // Build specifications
        return buildSpecifications(specificationSetEl, ns, version);
    }

    public static String marshal(YSpecification specification) {
        List<YSpecification> spLst = new ArrayList<YSpecification>();
        spLst.add(specification);
        return marshal(spLst, specification.getSchemaVersion());
    }
}
```

### 2.3 YSpecificationParser

The parser handles:
- **Metadata extraction**: Dublin Core elements (title, creator, version, etc.)
- **Schema parsing**: XSD types for data validation
- **Decomposition parsing**:
  - `NetFactsType` for composed nets
  - `WebServiceGatewayFactsType` for external services
- **Process control elements**: Input condition, tasks, conditions, output condition
- **Flow relationships**: Connections between elements with predicates

### 2.4 Version Detection

```java
private static YSchemaVersion getVersion(Element specRoot) {
    String version = specRoot.getAttributeValue("version");
    // version attribute was not mandatory in version 2
    return (null == version) ? YSchemaVersion.Beta2
                             : YSchemaVersion.fromString(version);
}
```

Supported versions:
- `Beta 2` (default when missing)
- `Beta 3`
- `Beta 4`
- `Beta 6`
- `Beta 7.1`
- `2.0`
- `2.1`
- `2.2`
- `3.0`
- `4.0` (current)

---

## 3. Specification Validation

### 3.1 Multi-Level Validation

YAWL employs three levels of validation:

```
┌─────────────────────────────────────────────────────────────────────────┐
│                     YAWL Validation Architecture                        │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  Level 1: XML Schema Validation (XSD)                                   │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │ - Validates structure against XSD                                 │   │
│  │ - Type checking (xs:string, xs:boolean, etc.)                    │   │
│  │ - Required attributes (id, type, etc.)                            │   │
│  │ - Cardinality constraints (minOccurs, maxOccurs)                 │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                              │                                         │
│                              ▼                                         │
│  Level 2: YDataValidator (Data Schema Validation)                      │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │ - Validates custom XSD types defined in specification           │   │
│  │ - Checks variable types against declared schema                  │   │
│  │ - Validates parameter mappings                                  │   │
│  │ - Namespace-aware validation                                    │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                              │                                         │
│                              ▼                                         │
│  Level 3: Semantic Validation (YVerificationHandler)                   │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │ - Directed path verification (i -> o connectivity)              │   │
│  │ - Decomposition usage (all decompositions referenced)            │   │
│  │ - Infinite loop detection                                       │   │
│  │ - Empty execution path detection                                │   │
│  │ - Parameter mapping completeness                                │   │
│  │ - Unique element IDs within nets                                │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

### 3.2 YSpecification.verify()

```java
public void verify(YVerificationHandler handler) {
    // Verify all decompositions
    for (YDecomposition decomposition : _decompositions.values()) {
        decomposition.verify(handler);
    }

    // Root net must exist
    if (_rootNet == null) {
        handler.error(this, "Specifications must have a root net.");
    }

    // Structural verification
    checkDecompositionUsage(handler);    // All decompositions used
    checkForInfiniteLoops(handler);       // No infinite loops without work
    checkForEmptyExecutionPaths(handler); // No paths without work generation
    checkDataTypesValidity(handler);      // Custom schema validation
}
```

### 3.3 YNet.verify()

```java
public void verify(YVerificationHandler handler) {
    super.verify(handler);

    // Must have input and output conditions
    if (_inputCondition == null) {
        handler.error(this, this + " must contain input condition.");
    }
    if (_outputCondition == null) {
        handler.error(this, this + " must contain output condition.");
    }

    // Only one input and one output condition
    for (YExternalNetElement element : _netElements.values()) {
        if (element instanceof YInputCondition && !element.equals(_inputCondition)) {
            handler.error(this, "Only one Input Condition allowed per net.");
        }
        if (element instanceof YOutputCondition && !element.equals(_outputCondition)) {
            handler.error(this, "Only one Output Condition allowed per net.");
        }
        element.verify(handler);
    }

    // All elements must be on directed path from i to o
    verifyDirectedPath(handler);

    // Local variable scoping
    new YNetLocalVarVerifier(this).verify(handler);
}
```

### 3.4 YTask.verify()

```java
public void verify(YVerificationHandler handler) {
    super.verify(handler);

    // Valid split and join types
    if (!(_splitType == _AND || _splitType == _OR || _splitType == _XOR)) {
        handler.error(this, this + " has an incorrect value for split type");
    }
    if (!(_joinType == _AND || _joinType == _OR || _joinType == _XOR)) {
        handler.error(this, this + " has an incorrect value for join type");
    }

    // OR/XOR splits must have exactly one default flow
    if (_splitType == _OR || _splitType == _XOR) {
        int defaultCount = 0;
        for (YFlow flow : getPostsetFlows()) {
            if (flow.isDefaultFlow()) defaultCount++;
        }
        if (defaultCount != 1) {
            handler.error(this, this + " the postset of any OR/XOR split must have" +
                    " exactly one default flow (not " + defaultCount + ")");
        }
    }

    // Remove set must reference valid elements in same net
    for (YExternalNetElement element : _removeSet) {
        if (element == null || !element._net.equals(_net)) {
            handler.error(this, this + " and " + element +
                " must be contained in the same net.");
        }
    }

    // Parameter mappings must match decomposition
    if (_decompositionPrototype != null) {
        checkParameterMappings(handler);
    }
}
```

### 3.5 Infinite Loop Detection

```java
private void checkForInfiniteLoops(YVerificationHandler handler) {
    // Check loops in root net (ERROR)
    Set<YExternalNetElement> relevantTasks =
        selectEmptyAndDecomposedTasks(Collections.singleton(_rootNet));
    checkTheseTasksForInfiniteLoops(relevantTasks, false, handler);

    // Check loops in unused nets (WARNING)
    Set<YDecomposition> netsBeingUsed = new HashSet<YDecomposition>();
    unfoldNetChildren(_rootNet, netsBeingUsed, null);
    Set<YDecomposition> unusedNets = new HashSet<YDecomposition>(_decompositions.values());
    unusedNets.removeAll(netsBeingUsed);
    relevantTasks = selectEmptyAndDecomposedTasks(unusedNets);
    checkTheseTasksForInfiniteLoops(relevantTasks, true, handler);
}
```

---

## 4. YAWL Java Class Hierarchy

### 4.1 Core Classes

```
YSpecification
│
├── YNet (extends YDecomposition)
│   ├── YInputCondition
│   ├── YOutputCondition
│   ├── YCondition
│   └── YExternalNetElement
│       └── YTask
│           ├── YCompositeTask
│           └── YAtomicTask
│               └── YMultiInstanceAttributes
│
└── YAWLServiceGateway (extends YDecomposition)
    └── External service invocations
```

### 4.2 YSpecification Class

**File:** `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/elements/YSpecification.java`

```java
public final class YSpecification implements Cloneable, YVerifiable {
    private String _specURI;                          // Specification identifier
    private YNet _rootNet;                            // Root decomposition
    private Map<String, YDecomposition> _decompositions; // All decompositions
    private String _name;                              // Specification name
    private String _documentation;                     // Description
    private YSchemaVersion _version;                   // Schema version
    private YDataValidator _dataValidator;             // Custom schema validator
    private YMetaData _metaData;                       // Dublin Core metadata
}
```

**Key Methods:**
- `getSpecificationID()`: Returns unique ID (identifier + version + URI)
- `verify(YVerificationHandler)`: Validates specification
- `toXML()`: Serializes to XML

### 4.3 YNet Class

**File:** `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/elements/YNet.java`

```java
public final class YNet extends YDecomposition {
    private YInputCondition _inputCondition;
    private YOutputCondition _outputCondition;
    private Map<String, YExternalNetElement> _netElements;
    private Map<String, YVariable> _localVariables;
    private String _externalDataGateway;
}
```

**Key Methods:**
- `getNetTasks()`: Returns all tasks in net
- `getPostset(Set)`: Static method to get postset elements
- `getPreset(Set)`: Static method to get preset elements
- `orJoinEnabled(YTask, YIdentifier)`: OR-join enabledness check

### 4.4 YTask Class

**File:** `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/elements/YTask.java`

```java
public abstract class YTask extends YExternalNetElement {
    public static final int _AND = 95;
    public static final int _OR = 103;
    public static final int _XOR = 126;

    private int _splitType;
    private int _joinType;
    private YMultiInstanceAttributes _multiInstAttr;
    private Set<YExternalNetElement> _removeSet;
    private Map<String, String> _dataMappingsForTaskStarting;
    private Map<String, String> _dataMappingsForTaskCompletion;
    private Map<String, String> _dataMappingsForTaskEnablement;
    private YDecomposition _decompositionPrototype;
    private YTimerParameters _timerParams;
    private Element _resourcingSpec;
}
```

**Key Methods:**
- `t_fire(YPersistenceManager)`: Fire the task
- `t_enabled(YIdentifier)`: Check if enabled
- `t_isBusy()`: Check if task is active
- `t_complete(...)`: Complete task execution

### 4.5 YDecomposition Class

**File:** `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/elements/YDecomposition.java`

```java
public abstract class YDecomposition implements Cloneable, YVerifiable {
    protected String _id;
    protected YSpecification _specification;
    private String _name;
    private String _documentation;
    private Map<String, YParameter> _inputParameters;
    private Map<String, YParameter> _outputParameters;
    private Map<String, YParameter> _enablementParameters;
    private Set<String> _outputExpressions;
    protected Document _data;
    private YAttributeMap _attributes;
    private boolean _manualInteraction;
    protected String _codelet;
}
```

### 4.6 YMetaData Class

**File:** `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/unmarshal/YMetaData.java`

```java
public class YMetaData {
    private String title;
    private List<String> creators;
    private List<String> subjects;
    private String description;
    private List<String> contributors;
    private String coverage;
    private Date validFrom;
    private Date validUntil;
    private Date created;
    private YSpecVersion version;
    private String status;
    private boolean persistent;
    private String uniqueID;  // null for pre-2.0 specs
}
```

**Dublin Core Elements:**
- `title`: Specification title
- `creator`: Author(s)
- `subject`: Keywords/tags
- `description`: Abstract
- `contributor`: Contributors
- `coverage`: Scope
- `validFrom`/`validUntil`: Validity period
- `created`: Creation date
- `version`: Specification version
- `status`: Current status
- `persistent`: Persistence flag
- `identifier`: Unique ID (v2.0+)

---

## 5. CRE YAML 0.2 Format

### 5.1 Root Structure

```yaml
yawl_yaml_version: "0.2"

specificationSet:
  yawl_schema_version: "2.1"
  uri: "workflow_identifier"
  metaData:
    title: "Workflow Title"
    version: "1.0"
    creator: "Author Name"
    description: "Description"

  rootNet: "NetId"

  roles:
    - "Role1"
    - "Role2"

  nets:
    - id: "NetId"
      type: "NetFacts"
      variables: [...]
      nodes: [...]
      flows: [...]
      regions: [...]
      subnets: [...]

  pattern_instances: [...]
  pattern_registry: {...}
  pattern_usage_index: {...}
```

### 5.2 Net Structure

```yaml
nets:
  - id: SimpleApproval
    type: NetFacts

    variables:
      - {name: request_id, type: string, initial: ""}
      - {name: request_amount, type: decimal, initial: 0.00}

    nodes:
      - {id: Start, kind: inputCondition}
      - {id: End, kind: outputCondition}
      - {id: TaskA, kind: task, taskType: human, name: "Task A"}
      - {id: TaskB, kind: task, taskType: automated}

    flows:
      - {from: Start, to: TaskA}
      - {from: TaskA, to: TaskB}
      - {from: TaskB, to: End}

    regions:
      - {id: Region1, cancel_region: true, description: "..."}
```

### 5.3 Pattern Instance Structure

```yaml
pattern_instances:
  # P1_Sequence
  - {id: P1_main, pattern: P1_Sequence, net: NetId,
     from: TaskA, to: TaskZ}

  # P4_ExclusiveChoice
  - {id: P4_choice, pattern: P4_ExclusiveChoice, net: NetId,
     at: DecisionTask, choices: [option1, option2]}

  # P19_CancelActivity
  - {id: P19_cancel, pattern: P19_CancelActivity, net: NetId,
     target: TaskId, cancel_event: withdrawal}

  # P21_StructuredLoop
  - {id: P21_loop, pattern: P21_StructuredLoop, net: NetId,
     entry: ValidateTask, body: [SubmitTask],
     exit_condition: "validated"}
```

### 5.4 Pattern Registry

```yaml
pattern_registry:
  P1_Sequence: {macro: "sequence"}
  P2_ParallelSplit: {macro: "and_split"}
  P3_Synchronization: {macro: "and_join"}
  P4_ExclusiveChoice: {macro: "xor_split"}
  P5_SimpleMerge: {macro: "xor_join"}
  # ... 43 YAWL patterns supported
```

### 5.5 CRE Implementation

**File:** `/Users/sac/cre/src/wf/wf_yaml_spec.erl`

```erlang
-record(yawl_yaml_spec, {
    id :: binary(),
    title :: binary(),
    version :: binary() | undefined,
    root_net :: binary(),
    tasks :: #{task_id() => task_info()},
    places :: [place()],
    transitions :: [transition()],
    decompositions :: #{binary() => decomposition_info()},
    flows :: [flow_info()],
    conditions :: #{binary() => condition_info()},
    % YAML-specific fields
    yaml_version :: binary(),
    roles :: [binary()],
    pattern_instances :: [pattern_instance()],
    pattern_registry :: #{binary() => pattern_registry_entry()},
    pattern_usage_index :: #{binary() => [binary()]},
    net_variables :: #{binary() => [variable_def()]},
    net_regions :: #{binary() => [region_def()]},
    net_subnets :: #{binary() => [subnet_def()]}
}).
```

**Key Parser Functions:**
- `from_yaml/1`: Parse YAML binary to specification
- `from_yaml_file/1`: Parse YAML file
- `validate/1`: Validate loaded specification

---

## 6. Schema Comparison

### 6.1 Structural Comparison

| Aspect | YAWL XML | CRE YAML 0.2 |
|--------|----------|--------------|
| **Root Element** | `<specificationSet>` | `specificationSet:` |
| **Version** | Attribute `version="4.0"` | Field `yawl_yaml_version: "0.2"` |
| **Namespace** | XML namespace (`xmlns`) | N/A (YAML conventions) |
| **Metadata** | `<metaData>` (Dublin Core) | `metaData:` (subset) |
| **Decompositions** | `<decomposition>` elements | `nets:` list |
| **Tasks** | `<task>` within processControlElements | `nodes:` with `kind: task` |
| **Conditions** | `<condition>` elements | `nodes:` with `kind: condition` |
| **Flows** | `<flowsInto>` within each element | `flows:` list at net level |
| **Variables** | `<localVariable>` elements | `variables:` list |
| **Split/Join** | `<split>`/`<join>` attributes | Not explicitly stored |
| **Mappings** | `<startingMappings>`/`<completedMappings>` | Not yet implemented |
| **Resourcing** | `<resourcing>` element | `roles:` list (simplified) |
| **Timer** | `<timer>` element | Not yet implemented |
| **MI Tasks** | `xsi:type="MultipleInstance..."` | Not yet implemented |

### 6.2 Feature Coverage Matrix

| Feature | YAWL XML Support | CRE YAML Support | Notes |
|---------|-----------------|-----------------|-------|
| Basic Tasks | Full | Full | |
| Composite Tasks | Full | Partial | Decomposition reference |
| Input/Output Conditions | Full | Full | |
| AND Split/Join | Full | Implicit | Must infer from flows |
| OR Split/Join | Full | Implicit | Must infer from flows |
| XOR Split/Join | Full | Implicit | Must infer from flows |
| Flow Predicates | Full | Full | XPath predicates |
| Data Variables | Full | Full | |
| Data Mappings | Full | Not yet | `startingMappings`/`completedMappings` |
| Multi-Instance | Full | Not yet | Minimum/maximum/threshold |
| Timers | Full | Not yet | Expiry triggers |
| Resourcing | Full | Partial | Roles only, no allocation |
| Web Services | Full | Not yet | WSDL operations |
| Codelets | Full | Not yet | Inline code execution |
| External Data Gateway | Full | Not yet | Database/ERP integration |
| Process Configuration | Full | Not yet | Dynamic port activation |
| Cancellation Regions | Full | Full | Via `regions:` |
| Patterns | Implicit | Full | Explicit `pattern_instances:` |
| Dublin Core Metadata | Full | Partial | Subset |
| Layout Information | Full (separate) | Not yet | Visual editor data |

### 6.3 Verbosity Comparison

**YAWL XML Example (simple task):**
```xml
<task id="decideName">
    <flowsInto>
        <nextElementRef id="decideSongs"/>
    </flowsInto>
    <join code="xor"/>
    <split code="and"/>
    <startingMappings>
        <mapping>
            <expression query="/OverseeMusic/nameOfArtist"/>
            <mapsTo>nameOfArtist</mapsTo>
        </mapping>
    </startingMappings>
    <completedMappings>
        <mapping>
            <expression query="/DecideAlbumName/nameOfRecord"/>
            <mapsTo>nameOfRecord</mapsTo>
        </mapping>
    </completedMappings>
    <decomposesTo id="DecideAlbumName"/>
</task>
```

**CRE YAML Equivalent (current):**
```yaml
- {id: decideName, kind: task, taskType: human}
```

**CRE YAML Equivalent (future - full):**
```yaml
- {id: decideName, kind: task, taskType: human,
   join: xor, split: and,
   starting_mappings:
     [{expr: "/OverseeMusic/nameOfArtist", mapsTo: "nameOfArtist"}],
   completed_mappings:
     [{expr: "/DecideAlbumName/nameOfRecord", mapsTo: "nameOfRecord"}],
   decomposesTo: DecideAlbumName}
```

### 6.4 Validation Comparison

| Validation Type | YAWL XML | CRE YAML |
|-----------------|----------|----------|
| Schema Validation | XSD-based | JSON Schema (planned) |
| Type Checking | XSD types | Erlang types (planned) |
| Structural Verification | YVerificationHandler | validate/1 |
| Infinite Loop Detection | Full | Not yet |
| Path Verification | Full (i -> o) | Not yet |
| Decomposition Usage | Full | Partial |
| Unique IDs | XSD key/keyref | Not yet |

---

## 7. Examples Side-by-Side

### 7.1 Simple Sequence

**YAWL XML:**
```xml
<decomposition id="SimpleSequence" isRootNet="true" xsi:type="NetFactsType">
    <processControlElements>
        <inputCondition id="input">
            <flowsInto><nextElementRef id="taskA"/></flowsInto>
        </inputCondition>
        <task id="taskA">
            <join code="xor"/>
            <split code="and"/>
            <flowsInto><nextElementRef id="taskB"/></flowsInto>
        </task>
        <task id="taskB">
            <join code="xor"/>
            <split code="and"/>
            <flowsInto><nextElementRef id="output"/></flowsInto>
        </task>
        <outputCondition id="output"/>
    </processControlElements>
</decomposition>
```

**CRE YAML:**
```yaml
nets:
  - id: SimpleSequence
    type: NetFacts
    nodes:
      - {id: input, kind: inputCondition}
      - {id: taskA, kind: task, taskType: human}
      - {id: taskB, kind: task, taskType: human}
      - {id: output, kind: outputCondition}
    flows:
      - {from: input, to: taskA}
      - {from: taskA, to: taskB}
      - {from: taskB, to: output}
```

**Savings:** ~60% reduction in lines, more readable structure

### 7.2 XOR Split with Predicate

**YAWL XML:**
```xml
<task id="selectSongs">
    <join code="and"/>
    <split code="xor"/>
    <flowsInto>
        <nextElementRef id="decideFormat"/>
        <isDefaultFlow/>
    </flowsInto>
    <flowsInto>
        <nextElementRef id="decideSongs"/>
        <predicate ordering="1">/OverseeMusic/proceed = 'false'</predicate>
    </flowsInto>
</task>
```

**CRE YAML:**
```yaml
flows:
  - {from: selectSongs, to: decideFormat}
  - {from: selectSongs, to: decideSongs,
     predicate: "/OverseeMusic/proceed = 'false'"}
```

### 7.3 Multi-Instance Task

**YAWL XML:**
```xml
<task id="record" xsi:type="MultipleInstanceExternalTaskFactsType">
    <join code="xor"/>
    <split code="and"/>
    <decomposesTo id="RecordSong"/>
    <minimum>1</minimum>
    <maximum>10</maximum>
    <threshold>4</threshold>
    <creationMode code="dynamic"/>
    <miDataInput>
        <expression query="&lt;songlist&gt;{...}&lt;/songlist&gt;"/>
        <splittingExpression query="for $d in /songlist/* return ..."/>
        <formalInputParam>songLocal</formalInputParam>
    </miDataInput>
    <miDataOutput>
        <formalOutputExpression query="/RecordSong/songLocal"/>
        <outputJoiningExpression query="&lt;songlist&gt;{...}&lt;/songlist&gt;"/>
        <resultAppliedToLocalVariable>songlist</resultAppliedToLocalVariable>
    </miDataOutput>
</task>
```

**CRE YAML (planned):**
```yaml
- {id: record, kind: task, taskType: human,
   multi_instance:
     {minimum: 1, maximum: 10, threshold: 4, creation_mode: dynamic,
      input: {expr: "<songlist>{...}</songlist>",
              splitting: "for $d in /songlist/* return ...",
              param: songLocal},
      output: {expr: "/RecordSong/songLocal",
               joining: "<songlist>{...}</songlist>",
               result_to: songlist}},
   decomposesTo: RecordSong}
```

### 7.4 Metadata

**YAWL XML:**
```xml
<metaData>
    <title>Cut Records</title>
    <creator>YAWL Foundation</creator>
    <subject>Music Production</subject>
    <description>A workflow for recording songs</description>
    <contributor>Contributor Name</contributor>
    <validFrom>2024-01-01</validFrom>
    <version>1.0</version>
    <status>Released</status>
    <persistent>true</persistent>
    <identifier>cut_records_v1</identifier>
</metaData>
```

**CRE YAML:**
```yaml
metaData:
  title: "Simple Single-Approver Workflow"
  version: "1.0"
  creator: "CRE Example Workflows"
  description: "Linear approval flow with single manager approval gate"
```

### 7.5 Variables

**YAWL XML:**
```xml
<localVariable>
    <name>songlist</name>
    <type>SonglistType</type>
</localVariable>
<localVariable>
    <name>proceed</name>
    <type>boolean</type>
    <namespace>http://www.w3.org/2001/XMLSchema</namespace>
    <initialValue>true</initialValue>
</localVariable>
```

**CRE YAML:**
```yaml
variables:
  - {name: songlist, type: SonglistType}
  - {name: proceed, type: boolean, initial: true}
```

### 7.6 Resourcing

**YAWL XML (complex):**
```xml
<resourcing>
    <offer initiator="system">
        <distributionSet>
            <initialSet>
                <param>
                    <name>role</name>
                    <refers>role</refers>
                </param>
            </initialSet>
            <filters>
                <filter name="familiarParticipant">
                    <name>familiarParticipant</name>
                    <params>
                        <param>
                            <key>taskID</key>
                            <value>previous_task</value>
                        </param>
                    </params>
                </filter>
            </filters>
        </distributionSet>
    </offer>
    <allocate initiator="user">
        <allocator>
            <name>allocate_by_least_busy</name>
        </allocator>
    </allocate>
    <start initiator="system"/>
    <privileges>
        <privilege>
            <name>canSuspend</name>
            <allowall>false</allowall>
        </privilege>
    </privileges>
</resourcing>
```

**CRE YAML (simplified):**
```yaml
roles:
  - Requester
  - Manager
  - System
```

---

## 8. Feature Mapping

### 8.1 Pattern Support

| YAWL Pattern | YAWL XML | CRE YAML | CRE Pattern Name |
|--------------|----------|----------|------------------|
| P1: Sequence | Implicit | Explicit | `P1_Sequence` |
| P2: Parallel Split | `split="and"` | Implicit | `P2_ParallelSplit` |
| P3: Synchronization | `join="and"` | Implicit | `P3_Synchronization` |
| P4: Exclusive Choice | `split="xor"` + predicates | Explicit | `P4_ExclusiveChoice` |
| P5: Simple Merge | `join="xor"` | Explicit | `P5_SimpleMerge` |
| P6: Multi-Choice | Multiple flows | Explicit | `P6_MultiChoice` |
| P7: Synchronizing Merge | Complex OR-join | Explicit | `P7_SynchronizingMerge` |
| P8: Multi-Merge | Implicit | Explicit | `P8_MultiMerge` |
| P9: Discriminator | Custom | Explicit | `P9_Discriminator` |
| P10: Arbitrary Cycles | Flows back | Explicit | `P10_ArbitraryCycles` |
| P11: Implicit Termination | Output condition | Explicit | `P11_ImplicitTermination` |
| P12: MI without a priori runtime knowledge | MI task | Not yet | `P12_MI_NoKnowledge` |
| P13: MI with a priori runtime knowledge | MI task | Not yet | `P13_MI_WithKnowledge` |
| P14: MI design time knowledge | Static MI | Not yet | `P14_MI_DesignTime` |
| ... | ... | ... | ... |

### 8.2 Task Type Mapping

| YAWL Task Type | XML Attribute | YAML Equivalent | Status |
|----------------|---------------|-----------------|--------|
| Empty Task | No decomposition | `taskType: empty` | Implicit |
| Atomic Task | WebServiceGateway | `taskType: automated` | Supported |
| Composite Task | Net decomposition | `taskType: human` + subnet | Partial |
| Multi-Instance | `xsi:type="MultipleInstance..."` | `multi_instance:` | Planned |

### 8.3 Data Mapping

| Mapping Type | YAWL XML | CRE YAML | Status |
|--------------|----------|----------|--------|
| Input Mapping | `<startingMappings>` | `starting_mappings:` | Planned |
| Output Mapping | `<completedMappings>` | `completed_mappings:` | Planned |
| Enablement Mapping | `<enablementMappings>` | `enablement_mappings:` | Planned |
| Variable Types | `<xs:schema>` | Type definitions | Partial |

---

## 9. Recommendations

### 9.1 For CRE YAML Format Enhancement

1. **Add Split/Join Explicit Storage**
   - Currently inferred from flows
   - Should store explicitly for clarity
   - Enables validation against YAWL semantics

2. **Implement Data Mappings**
   - Add `starting_mappings` and `completed_mappings` to tasks
   - Support XPath/XQuery expressions
   - Validate decomposition parameter usage

3. **Add Multi-Instance Support**
   - Define `multi_instance` task property
   - Support minimum/maximum/threshold
   - Handle creation mode (static/dynamic)

4. **Implement Timer Support**
   - Add `timer` property to tasks
   - Support trigger types (OnEnabled/OnExecuting)
   - Support duration and expiry time

5. **Enhance Resourcing**
   - Move beyond simple `roles:` list
   - Support distribution sets, filters, constraints
   - Add allocation and initiation strategies

6. **Add Web Service Gateway Type**
   - Support WSDL location and operation
   - Enable codelet execution
   - Support external interaction flags

### 9.2 For Parser/Validation Enhancement

1. **Implement Full YVerificationHandler**
   - Add infinite loop detection
   - Implement directed path verification
   - Check decomposition usage

2. **Add Schema Validation**
   - Create JSON Schema for YAML 0.2
   - Validate types and constraints
   - Support custom type definitions

3. **Support Schema Version Detection**
   - Auto-detect YAWL schema version from XML
   - Enable migration between versions
   - Support hybrid XML/YAML workflows

### 9.3 Migration Path

1. **Phase 1: Parity** (Current + 6 months)
   - Complete pattern instance support
   - Add explicit split/join storage
   - Implement data mappings

2. **Phase 2: Feature Parity** (6-12 months)
   - Multi-instance tasks
   - Timer support
   - Extended resourcing

3. **Phase 3: Advanced Features** (12-18 months)
   - Web service gateways
   - External data gateways
   - Process configuration

4. **Phase 4: Tooling** (18+ months)
   - YAML to XML converter
   - XML to YAML converter
   - Visual editor support

---

## Appendix A: YAWL XSD Schema Location

**File:** `/Users/sac/cre/vendors/yawl/schema/YAWL_Schema4.0.xsd`

The XSD schema defines:
- 1234 lines of schema definitions
- Root element: `specificationSet`
- Target namespace: `http://www.yawlfoundation.org/yawlschema`
- Supports YAWL schema version 4.0

Key complex types:
- `YAWLSpecificationFactsType`
- `NetFactsType`
- `WebServiceGatewayFactsType`
- `ExternalTaskFactsType`
- `MultipleInstanceExternalTaskFactsType`
- `MetaDataType`
- `VariableFactsType`

---

## Appendix B: YAWL Source Files Reference

| Class | Location |
|-------|----------|
| `YSpecification` | `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/elements/YSpecification.java` |
| `YNet` | `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/elements/YNet.java` |
| `YTask` | `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/elements/YTask.java` |
| `YDecomposition` | `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/elements/YDecomposition.java` |
| `YMetaData` | `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/unmarshal/YMetaData.java` |
| `YMarshal` | `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/unmarshal/YMarshal.java` |
| `YSpecificationParser` | `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/unmarshal/YSpecificationParser.java` |

---

## Appendix C: CRE Source Files Reference

| Module | Location |
|--------|----------|
| YAML Parser | `/Users/sac/cre/src/wf/wf_yaml_spec.erl` |
| YAML Executor | `/Users/sac/cre/src/wf/wf_yawl_executor.erl` |
| Pattern Expander | `/Users/sac/cre/src/core/yawl_pattern_expander.erl` |
| YAWL Compiler | `/Users/sac/cre/src/core/yawl_compile.erl` |
| Example Workflows | `/Users/sac/cre/docs/example_workflows/*.yaml` |

---

## Document Metadata

- **Created:** 2025-02-07
- **Author:** CRE Analysis Team
- **Version:** 1.0
- **Status:** Final
- **Related Documents:**
  - `/Users/sac/cre/docs/bcd_templates.md`
  - `/Users/sac/cre/docs/pattern_reference_card.md`
  - YAWL 4.0 XSD Schema
  - CRE YAML 0.2 Specification
