# -----------------------------------------------------------------------------
# Variables - Binary Authorization Module
# -----------------------------------------------------------------------------

variable "project_id" {
  description = "GCP project ID where CRE is deployed"
  type        = string
}

variable "cluster_name" {
  description = "GKE cluster resource name (full resource path)"
  type        = string
  # Example: "projects/my-project/locations/us-central1/clusters/cre-cluster"
}

variable "enforcement_mode" {
  description = "Binary Authorization enforcement mode"
  type        = string

  validation {
    condition     = contains(["ENFORCED_AND_BLOCKING", "ENFORCED_AND_AUDIT_ONLY", "DISABLED"], var.enforcement_mode)
    error_message = "enforcement_mode must be one of: ENFORCED_AND_BLOCKING, ENFORCED_AND_AUDIT_ONLY, DISABLED."
  }

  default = "ENFORCED_AND_BLOCKING"
}

variable "cosign_public_key_id" {
  description = "Cosign public key ID for signature verification (e.g., 'https://raw.githubusercontent.com/user/repo/main/cosign.pub' or 'gs://bucket/cosign.pub')"
  type        = string
  default     = null
}

variable "allowlist_patterns" {
  description = "List of image patterns to allow without signature verification (use with caution for development/testing)"
  type        = list(string)
  default     = []

  # Example:
  # [
  #   "us-central1-docker.pkg.dev/my-project/cre/*",
  #   "gcr.io/distroless/*"
  # ]
}

variable "enable_on_gke_cluster" {
  description = "Enable Binary Authorization on the specified GKE cluster (must be set during cluster creation)"
  type        = bool
  default     = true
}
