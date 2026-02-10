# -----------------------------------------------------------------------------
# Variables - GCP Security Module
# -----------------------------------------------------------------------------

variable "project_id" {
  description = "GCP project ID where resources will be created"
  type        = string
}

variable "project_number" {
  description = "GCP project number for Workload Identity Federation configuration (optional)"
  type        = string
  default     = ""
}

variable "iam_project_id" {
  description = "Project ID for IAM resources (use shared project if different)"
  type        = string
  default     = ""
}

variable "region" {
  description = "GCP region for resources"
  type        = string
}

variable "name_prefix" {
  description = "Prefix for resource names"
  type        = string
}

variable "cluster_name" {
  description = "GKE cluster name for Workload Identity Federation"
  type        = string
}

# -----------------------------------------------------------------------------
# GitHub Actions Configuration
# -----------------------------------------------------------------------------
variable "github_pool_id" {
  description = "Workload Identity Pool ID for GitHub Actions"
  type        = string
  default     = "github-actions-pool"
}

variable "github_repository" {
  description = "GitHub repository in format 'owner/repo' for Workload Identity"
  type        = string
}

variable "additional_github_repositories" {
  description = "Additional GitHub repositories to grant access"
  type        = list(string)
  default     = []
}

variable "github_attribute_condition" {
  description = "Attribute condition for GitHub OIDC provider"
  type        = string
  default     = ""
}

variable "enable_branch_deployment" {
  description = "Enable branch-based deployment access"
  type        = bool
  default     = false
}

# -----------------------------------------------------------------------------
# GKE Configuration
# -----------------------------------------------------------------------------
variable "gke_namespace" {
  description = "Kubernetes namespace for CRE deployment"
  type        = string
  default     = "cre"
}

variable "kubernetes_service_account" {
  description = "Kubernetes service account name for Workload Identity"
  type        = string
  default     = "cre-sa"
}

variable "additional_kubernetes_namespaces" {
  description = "Additional Kubernetes namespaces to grant Workload Identity access"
  type        = list(string)
  default     = []
}

variable "gke_cluster_issuer_uri" {
  description = "OIDC issuer URI for GKE cluster (optional, auto-detected if empty)"
  type        = string
  default     = ""
}

variable "gke_allowed_audiences" {
  description = "Allowed audiences for GKE OIDC provider"
  type        = list(string)
  default     = []
}

variable "gke_attribute_condition" {
  description = "Attribute condition for GKE OIDC provider"
  type        = string
  default     = ""
}

# -----------------------------------------------------------------------------
# Secret Management
# -----------------------------------------------------------------------------
variable "erlang_cookie_value" {
  description = "Erlang cookie value (leave empty for auto-generation)"
  type        = string
  sensitive   = true
  default     = ""
}

variable "create_erlang_cookie_version" {
  description = "Create initial secret version for Erlang cookie"
  type        = bool
  default     = true
}

variable "enable_auto_rotation" {
  description = "Enable automatic secret rotation (requires external rotation job)"
  type        = bool
  default     = false
}

variable "secret_version_ttl" {
  description = "Time-to-live for secret versions before automatic destruction"
  type        = string
  default     = "604800s" # 7 days
}

variable "enable_database_secrets" {
  description = "Enable database-related secrets"
  type        = bool
  default     = false
}

variable "api_secret_names" {
  description = "Names for API token secrets to create"
  type        = list(string)
  default     = []
}

# -----------------------------------------------------------------------------
# Network Policy
# -----------------------------------------------------------------------------
variable "enable_default_deny_policies" {
  description = "Enable default-deny ingress and egress policies (recommended for production)"
  type        = bool
  default     = false
}

variable "cre_app_selector" {
  description = "Label selector for CRE application pods"
  type        = map(string)
  default = {
    app = "cre"
  }
}

variable "erlang_port_range" {
  description = "Erlang distribution port range"
  type        = list(number)
  # Note: range() cannot be used in variable defaults
  # Use a list instead or provide via locals
  default = []
}

variable "enable_istio" {
  description = "Enable Istio service mesh integration"
  type        = bool
  default     = false
}

variable "health_check_cidr" {
  description = "CIDR block for GCP health check IPs"
  type        = string
  default     = "130.211.0.0/22"
}

variable "cre_app_port" {
  description = "Main application port for incoming traffic"
  type        = number
  default     = 8080
}

variable "cre_health_check_port" {
  description = "Health check port for CRE application"
  type        = number
  default     = 8081
}

variable "private_network_cidrs" {
  description = "Private network CIDRs to exclude from egress rules"
  type        = list(string)
  default     = ["10.0.0.0/8", "172.16.0.0/12", "192.168.0.0/16"]
}

variable "database_cidr" {
  description = "CIDR block for database access"
  type        = string
  default     = "0.0.0.0/0"
}

variable "database_port" {
  description = "Database port for egress rules"
  type        = number
  default     = 5432
}

# -----------------------------------------------------------------------------
# Pod Security
# -----------------------------------------------------------------------------
variable "enable_pod_security_policy" {
  description = "Enable Pod Security Policy (deprecated, use Pod Security Standards)"
  type        = bool
  default     = false
}

variable "create_namespace" {
  description = "Create the Kubernetes namespace with security labels"
  type        = bool
  default     = false
}

variable "pod_security_enforce_level" {
  description = "Pod Security enforce level"
  type        = string
  default     = "baseline"

  validation {
    condition     = contains(["privileged", "baseline", "restricted"], var.pod_security_enforce_level)
    error_message = "pod_security_enforce_level must be one of: privileged, baseline, restricted"
  }
}

variable "pod_security_audit_level" {
  description = "Pod Security audit level"
  type        = string
  default     = "restricted"

  validation {
    condition     = contains(["privileged", "baseline", "restricted"], var.pod_security_audit_level)
    error_message = "pod_security_audit_level must be one of: privileged, baseline, restricted"
  }
}

variable "pod_security_warn_level" {
  description = "Pod Security warn level"
  type        = string
  default     = "restricted"

  validation {
    condition     = contains(["privileged", "baseline", "restricted"], var.pod_security_warn_level)
    error_message = "pod_security_warn_level must be one of: privileged, baseline, restricted"
  }
}

# -----------------------------------------------------------------------------
# Admin Access
# -----------------------------------------------------------------------------
variable "enable_admin_impersonation" {
  description = "Enable admin impersonation of service accounts"
  type        = bool
  default     = false
}

variable "admin_impersonator_email" {
  description = "Email address of admin impersonator"
  type        = string
  default     = ""
}

# -----------------------------------------------------------------------------
# Common Labels
# -----------------------------------------------------------------------------
variable "common_labels" {
  description = "Common labels to apply to all resources"
  type        = map(string)
  default = {
    managed_by = "terraform"
    component  = "security"
  }
}

# -----------------------------------------------------------------------------
# Identity-Aware Proxy (IAP) Configuration
# -----------------------------------------------------------------------------
variable "iap_config" {
  description = "IAP configuration for securing external API access"
  type = object({
    enabled                  = bool
    backend_service_name     = string
    oauth_brand_id           = string
    allowed_users            = list(string)
    allowed_groups           = list(string)
    allowed_service_accounts = list(string)
    authorized_domain        = string
    enable_audit_logs        = bool
    enable_services          = bool
    create_access_level      = bool
    access_policy_id         = string
    access_level_ip_ranges   = list(string)
    require_device_trust     = bool
    create_bypass_sa         = bool
    bypass_caller_sa         = string
  })
  default = {
    enabled                  = false
    backend_service_name     = ""
    oauth_brand_id           = ""
    allowed_users            = []
    allowed_groups           = []
    allowed_service_accounts = []
    authorized_domain        = ""
    enable_audit_logs        = true
    enable_services          = true
    create_access_level      = false
    access_policy_id         = ""
    access_level_ip_ranges   = []
    require_device_trust     = false
    create_bypass_sa         = false
    bypass_caller_sa         = ""
  }
}

variable "iap_allowed_users" {
  description = "List of user emails allowed to access IAP-secured resources (legacy, use iap_config)"
  type        = list(string)
  default     = []
}

variable "iap_oauth_brand_id" {
  description = "OAuth brand ID for IAP (use gcloud iap oauth-brands list to find)"
  type        = string
  default     = ""
}

variable "iap_backend_service" {
  description = "Name of the backend service to secure with IAP"
  type        = string
  default     = ""
}

# -----------------------------------------------------------------------------
# Kubernetes Provider Configuration
# -----------------------------------------------------------------------------
variable "cluster_endpoint" {
  description = "GKE cluster endpoint for Kubernetes provider (optional, auto-discovered if empty)"
  type        = string
  default     = ""
}

variable "cluster_token" {
  description = "Authentication token for Kubernetes provider (optional, uses ADC if empty)"
  type        = string
  sensitive   = true
  default     = ""
}

variable "cluster_ca_certificate" {
  description = "Base64 encoded cluster CA certificate (optional, auto-discovered if empty)"
  type        = string
  default     = ""
}

# -----------------------------------------------------------------------------
# Branch Deployment Configuration
# -----------------------------------------------------------------------------
variable "enable_branch_deployments" {
  description = "Enable branch-based deployment access"
  type        = bool
  default     = false
}
