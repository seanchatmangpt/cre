# CRE Makefile
# Common Runtime Environment - YAWL Workflow Engine

.PHONY: all compile clean test dialyzer dot escript f5_validate help

# Default target
all: compile

# Compile the project
compile:
	@echo "Compiling CRE..."
	rebar3 compile

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	rebar3 clean

# Run all tests
test:
	@echo "Running tests..."
	rebar3 eunit

# Run dialyzer for static analysis
dialyzer:
	@echo "Running dialyzer..."
	rebar3 dialyzer

# Build the dot escript CLI
dot: escript

escript:
	@echo "Building dot escript..."
	rebar3 as prod escriptize
	@if [ -f "_build/prod/bin/dot" ]; then cp _build/prod/bin/dot dot && chmod +x dot && echo "dot escript built successfully"; else echo "Warning: dot escript not found"; fi

# Run F5 validation
f5_validate: dot
	@echo "Running F5 validation..."
	./scripts/f5_validate.sh --verbose

# Build Docker image
docker:
	@echo "Building Docker image..."
	docker buildx bake --load

# Run platform-agnostic CI/CD pipeline (Build → SBOM → Scan → optional Push)
cicd:
	@echo "Running CI/CD pipeline..."
	./scripts/cicd-pipeline.sh

# Run F5 validation in Docker
f5_validate_docker: docker
	@echo "Running F5 validation in Docker..."
	docker run --rm -v "$(PWD):/work:rw" -w /work cre:0.3.0 sh -c "chmod +x scripts/f5_validate.sh && scripts/f5_validate.sh --verbose"

# Display help
help:
	@echo "CRE Makefile targets:"
	@echo ""
	@echo "  all              - Compile the project (default)"
	@echo "  compile          - Compile with rebar3"
	@echo "  clean            - Clean build artifacts"
	@echo "  test             - Run all tests"
	@echo "  dialyzer         - Run static analysis"
	@echo "  dot / escript    - Build the dot escript CLI"
	@echo "  f5_validate      - Run F5 validation suite"
	@echo "  docker           - Build Docker image"
	@echo "  cicd             - Run CI/CD pipeline (Build → SBOM → Scan)"
	@echo "  f5_validate_docker - Run F5 validation in Docker"
	@echo ""
	@echo "Dot command usage (after running 'make dot'):"
	@echo "  ./dot validate <module>   - Validate workflow specifications"
	@echo "  ./dot sync                - Synchronize evidence"
	@echo "  ./dot evidence            - Collect evidence pack"
	@echo "  ./dot prove <module>      - Run proof verification"
	@echo "  ./dot bench               - Run benchmarks"
	@echo "  ./dot andon               - Check andon gate status"
	@echo ""
