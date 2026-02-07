# GCP Marketplace Deployer - Makefile

# Variables
PROJECT_ID ?= $(shell gcloud config get-value project)
APP_NAME ?= marketplace-app
APP_VERSION ?= 1.0.0
REGISTRY ?= gcr.io
IMAGE_NAME = $(REGISTRY)/$(PROJECT_ID)/$(APP_NAME)/deployer
IMAGE_TAG ?= $(APP_VERSION)
REGION ?= us-central1

# Colors for output
BLUE := \033[0;34m
GREEN := \033[0;32m
RED := \033[0;31m
NC := \033[0m # No Color

.PHONY: help
help: ## Show this help message
	@echo '$(BLUE)GCP Marketplace Deployer$(NC)'
	@echo ''
	@echo 'Usage:'
	@echo '  make $(GREEN)<target>$(NC)'
	@echo ''
	@echo 'Targets:'
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "  $(GREEN)%-20s$(NC) %s\n", $$1, $$2}' $(MAKEFILE_LIST)

.PHONY: validate
validate: ## Validate prerequisites
	@echo "$(BLUE)Validating prerequisites...$(NC)"
	@command -v docker >/dev/null 2>&1 || { echo "$(RED)docker is required but not installed$(NC)"; exit 1; }
	@command -v gcloud >/dev/null 2>&1 || { echo "$(RED)gcloud is required but not installed$(NC)"; exit 1; }
	@command -v kubectl >/dev/null 2>&1 || { echo "$(RED)kubectl is required but not installed$(NC)"; exit 1; }
	@[ -n "$(PROJECT_ID)" ] || { echo "$(RED)PROJECT_ID is not set$(NC)"; exit 1; }
	@echo "$(GREEN)✓ All prerequisites validated$(NC)"

.PHONY: enable-apis
enable-apis: ## Enable required GCP APIs
	@echo "$(BLUE)Enabling required GCP APIs...$(NC)"
	gcloud services enable \
		compute.googleapis.com \
		container.googleapis.com \
		sqladmin.googleapis.com \
		iam.googleapis.com \
		servicenetworking.googleapis.com \
		cloudresourcemanager.googleapis.com \
		--project=$(PROJECT_ID)
	@echo "$(GREEN)✓ APIs enabled$(NC)"

.PHONY: build
build: validate ## Build the deployer container
	@echo "$(BLUE)Building deployer image...$(NC)"
	docker build -t $(IMAGE_NAME):$(IMAGE_TAG) .
	docker tag $(IMAGE_NAME):$(IMAGE_TAG) $(IMAGE_NAME):latest
	@echo "$(GREEN)✓ Image built: $(IMAGE_NAME):$(IMAGE_TAG)$(NC)"

.PHONY: push
push: build ## Push the deployer container to registry
	@echo "$(BLUE)Pushing image to registry...$(NC)"
	gcloud auth configure-docker
	docker push $(IMAGE_NAME):$(IMAGE_TAG)
	docker push $(IMAGE_NAME):latest
	@echo "$(GREEN)✓ Image pushed: $(IMAGE_NAME):$(IMAGE_TAG)$(NC)"

.PHONY: test-local
test-local: build ## Test deployer locally (requires gcloud auth)
	@echo "$(BLUE)Testing deployer locally...$(NC)"
	@echo "$(BLUE)Using PROJECT_ID: $(PROJECT_ID)$(NC)"
	docker run --rm \
		-e PROJECT_ID=$(PROJECT_ID) \
		-e REGION=$(REGION) \
		-e APP_NAME=$(APP_NAME)-test \
		-e NAMESPACE=test \
		-e DRY_RUN=true \
		-v ~/.config/gcloud:/root/.config/gcloud:ro \
		$(IMAGE_NAME):$(IMAGE_TAG)

.PHONY: deploy-test
deploy-test: push ## Deploy to test environment
	@echo "$(BLUE)Deploying to test environment...$(NC)"
	kubectl apply -f manifests/test/
	@echo "$(GREEN)✓ Deployed to test$(NC)"

.PHONY: verify-schema
verify-schema: ## Verify schema.yaml syntax
	@echo "$(BLUE)Verifying schema.yaml...$(NC)"
	@command -v yamllint >/dev/null 2>&1 && yamllint schema.yaml || echo "$(RED)yamllint not found, skipping$(NC)"
	@python3 -c "import yaml; yaml.safe_load(open('schema.yaml'))" && echo "$(GREEN)✓ Schema is valid$(NC)" || echo "$(RED)✗ Schema is invalid$(NC)"

.PHONY: lint
lint: ## Lint shell scripts
	@echo "$(BLUE)Linting shell scripts...$(NC)"
	@command -v shellcheck >/dev/null 2>&1 && \
		find scripts -name "*.sh" -exec shellcheck {} + && \
		echo "$(GREEN)✓ All scripts passed linting$(NC)" || \
		echo "$(RED)shellcheck not found, skipping$(NC)"

.PHONY: clean
clean: ## Clean up local images
	@echo "$(BLUE)Cleaning up local images...$(NC)"
	docker rmi $(IMAGE_NAME):$(IMAGE_TAG) $(IMAGE_NAME):latest 2>/dev/null || true
	@echo "$(GREEN)✓ Cleaned up$(NC)"

.PHONY: package
package: push verify-schema ## Package for GCP Marketplace submission
	@echo "$(BLUE)Packaging for GCP Marketplace...$(NC)"
	@mkdir -p package
	@cp schema.yaml package/
	@cp README.md package/
	@echo "image: $(IMAGE_NAME):$(IMAGE_TAG)" > package/deployer.yaml
	@tar -czf $(APP_NAME)-$(APP_VERSION).tar.gz package/
	@echo "$(GREEN)✓ Package created: $(APP_NAME)-$(APP_VERSION).tar.gz$(NC)"
	@rm -rf package

.PHONY: version
version: ## Show version information
	@echo "App Name:    $(APP_NAME)"
	@echo "Version:     $(APP_VERSION)"
	@echo "Image:       $(IMAGE_NAME):$(IMAGE_TAG)"
	@echo "Project:     $(PROJECT_ID)"
	@echo "Registry:    $(REGISTRY)"

.PHONY: docs
docs: ## Generate documentation
	@echo "$(BLUE)Generating documentation...$(NC)"
	@echo "# Deployment Scripts Documentation" > docs/SCRIPTS.md
	@echo "" >> docs/SCRIPTS.md
	@for script in scripts/*.sh; do \
		echo "## $$(basename $$script)" >> docs/SCRIPTS.md; \
		echo "" >> docs/SCRIPTS.md; \
		grep -E "^#" $$script | head -5 >> docs/SCRIPTS.md || true; \
		echo "" >> docs/SCRIPTS.md; \
	done
	@echo "$(GREEN)✓ Documentation generated$(NC)"

.PHONY: install-tools
install-tools: ## Install required development tools
	@echo "$(BLUE)Installing development tools...$(NC)"
	pip3 install yamllint
	@echo "For shellcheck, install via your package manager:"
	@echo "  Ubuntu/Debian: apt-get install shellcheck"
	@echo "  macOS: brew install shellcheck"

# Default target
.DEFAULT_GOAL := help
