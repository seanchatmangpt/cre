# CRE Architecture Diagrams

This directory contains architecture and deployment diagrams for CRE on Google Cloud Marketplace.

## Diagrams

### Architecture Diagram
- **architecture.png** - High-level CRE system architecture
  - Shows component layers (Application, OTP Runner, Pure Helpers)
  - Data flow and integration points
  - Source: `../../docs/diagrams/c4/level1-container-diagram-v2.puml`

### Deployment Diagram
- **deployment.png** - GKE deployment architecture
  - Shows pod structure, services, and load balancer
  - Shows Mnesia replication across pods
  - Shows Google Cloud integration points
  - Source: `deployment.puml`

### Component Diagram
- **components.png** - Internal CRE component architecture
  - Shows gen_pnet, gen_yawl, and pattern implementations
  - Shows pure helper modules
  - Source: `../../docs/diagrams/c4/level2-yawl-engine-component.puml`

## Exporting Diagrams

Diagrams are created with PlantUML. To export or modify:

### Install PlantUML

**macOS**:
```bash
brew install plantuml
```

**Ubuntu/Debian**:
```bash
sudo apt-get install plantuml
```

**Docker**:
```bash
docker run -v $(pwd):/data plantuml/plantuml diagram.puml
```

### Export to PNG

```bash
# Navigate to diagrams directory
cd marketplace/listing-package/diagrams

# Export architecture diagram
plantuml ../../docs/diagrams/c4/level1-container-diagram-v2.puml -o architecture.png

# Export deployment diagram
plantuml deployment.puml

# Export components diagram
plantuml ../../docs/diagrams/c4/level2-yawl-engine-component.puml -o components.png
```

### Export to SVG (for web)

```bash
plantuml deployment.puml -tsvg
```

## Diagram Standards

- **Format**: PNG for Marketplace submission, SVG for web
- **Resolution**: Minimum 1024x768 pixels
- **Font Size**: 12-14pt for readability
- **Colors**: Use CRE brand colors (blue, gray, white)
- **Labels**: Clear, concise labels with technical terms

## Updating Diagrams

When updating CRE architecture:

1. Modify source PlantUML files in `docs/diagrams/`
2. Export updated PNG files to this directory
3. Update documentation references
4. Commit updated diagrams

---

**Version**: 0.3.0
**Last Updated**: 2025-01-10
