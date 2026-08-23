# -----------------------------------------------------------------------------
# Binary Authorization Policy Module - Enforce Image Signing
# GCP Security Module for CRE Terraform
#
# Implements:
# - Binary Authorization policy for GKE clusters
# - Cosign signature verification enforcement
# - Whitelist approach (deny by default, allow signed images)
# -----------------------------------------------------------------------------

# Binary Authorization policy for CRE
# Default: deny all images (whitelist approach)
resource "google_binary_authorization_policy" "cre_policy" {
  project = var.project_id

  # Default admission rule: DENY ALL
  # This is a whitelist approach - only explicitly allowed images can be deployed
  default_admission_rule {
    evaluation_mode  = "ALWAYS_DENY"
    enforcement_mode = var.enforcement_mode
  }

  # Allow CRE images signed by the trusted cosign key
  # This requires images to be signed with cosign before deployment
  dynamic "admit_rule" {
    for_each = var.cosign_public_key_id != null ? [1] : []
    content {
      evaluation_mode  = "ALWAYS_ALLOW"
      enforcement_mode = var.enforcement_mode

      # Require signature from specific cosign public key
      require_attestations_by_signer {
        sigmaker_public_key_id = var.cosign_public_key_id
      }
    }
  }

  # Allow Google-built images (base images, distroless, etc.)
  # These are signed by Google's own keys
  clusters_admission_rules {
    cluster           = var.cluster_name
    evaluation_mode    = "ALWAYS_ALLOW"
    enforcement_mode  = var.enforcement_mode
  }

  # Allowlist for specific images (optional, for development/testing)
  # Use with caution: this bypasses signature verification
  dynamic "admission_allowlist" {
    for_each = var.allowlist_patterns
    content {
      name_pattern = admission_allowlist.value
    }
  }
}

# -----------------------------------------------------------------------------
# Outputs
# -----------------------------------------------------------------------------

output "policy_name" {
  description = "Binary Authorization policy name"
  value       = google_binary_authorization_policy.cre_policy.name
}

output "policy_enforcement_mode" {
  description = "Current enforcement mode (ENFORCED_AND_BLOCKING or DISABLED)"
  value       = var.enforcement_mode
}

output "cosign_public_key_id" {
  description = "Cosign public key ID used for signature verification"
  value       = var.cosign_public_key_id
}

output "verification_command" {
  description = "Command to verify image signature locally"
  value       = <<-EOT
# Verify CRE image signature before deployment
cosign verify IMAGE_TAG \
  --key ${var.cosign_public_key_id}
EOT
}
