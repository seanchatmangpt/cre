# Binary Authorization for GKE
# Ensures only trusted container images are deployed

locals {
  binauth_enabled = var.enable_binary_authorization
}

# Binary Authorization Policy
resource "google_binary_authorization_policy" "cre_policy" {
  count = local.binauth_enabled ? 1 : 0

  # Default rule: DENY all images unless explicitly allowed
  default_admission_rule {
    evaluation_mode  = "REQUIRE_ATTESTATION"
    enforcement_mode = "ENFORCED_BLOCK_AND_AUDIT_LOG"

    require_attestations_by = [
      google_binary_authorization_attestor.build_attestor[0].name,
      google_binary_authorization_attestor.security_attestor[0].name,
    ]
  }

  # Cluster-specific admission rules
  dynamic "cluster_admission_rules" {
    for_each = var.enable_binary_authorization ? [1] : []

    content {
      cluster                 = google_container_cluster.primary.id
      evaluation_mode         = "REQUIRE_ATTESTATION"
      enforcement_mode        = "ENFORCED_BLOCK_AND_AUDIT_LOG"

      require_attestations_by = [
        google_binary_authorization_attestor.build_attestor[0].name,
        google_binary_authorization_attestor.security_attestor[0].name,
      ]
    }
  }

  # Admission allowlist: GKE system images
  admission_whitelist_patterns {
    name_pattern = "gcr.io/google-containers/*"
  }

  admission_whitelist_patterns {
    name_pattern = "gcr.io/gke-release/*"
  }

  admission_whitelist_patterns {
    name_pattern = "gke.gcr.io/*"
  }

  # Development namespace exemption (optional, use with caution)
  dynamic "admission_whitelist_patterns" {
    for_each = var.binauth_allow_dev_images ? [1] : []

    content {
      name_pattern = "${var.artifact_registry_region}-docker.pkg.dev/${var.project_id}/dev/*"
    }
  }

  # Global policy evaluation mode
  global_policy_evaluation_mode = "ENABLE"
}

# Attestor: Build Verification
resource "google_binary_authorization_attestor" "build_attestor" {
  count = local.binauth_enabled ? 1 : 0

  name        = "cre-build-attestor"
  description = "Attestor for CI/CD build verification"

  attestation_authority_note {
    note_reference = google_container_analysis_note.build_note[0].name

    public_keys {
      id = data.google_kms_crypto_key_version.attestor_key_version[0].id

      pkix_public_key {
        public_key_pem      = data.google_kms_crypto_key_version.attestor_key_version[0].public_key[0].pem
        signature_algorithm = data.google_kms_crypto_key_version.attestor_key_version[0].public_key[0].algorithm
      }
    }
  }
}

# Attestor: Security Scan Verification
resource "google_binary_authorization_attestor" "security_attestor" {
  count = local.binauth_enabled ? 1 : 0

  name        = "cre-security-attestor"
  description = "Attestor for security scan verification"

  attestation_authority_note {
    note_reference = google_container_analysis_note.security_note[0].name

    public_keys {
      id = data.google_kms_crypto_key_version.security_attestor_key_version[0].id

      pkix_public_key {
        public_key_pem      = data.google_kms_crypto_key_version.security_attestor_key_version[0].public_key[0].pem
        signature_algorithm = data.google_kms_crypto_key_version.security_attestor_key_version[0].public_key[0].algorithm
      }
    }
  }
}

# Container Analysis Note for Build Attestations
resource "google_container_analysis_note" "build_note" {
  count = local.binauth_enabled ? 1 : 0

  name = "cre-build-attestation-note"

  attestation_authority {
    hint {
      human_readable_name = "CRE CI/CD Build Verification"
    }
  }
}

# Container Analysis Note for Security Attestations
resource "google_container_analysis_note" "security_note" {
  count = local.binauth_enabled ? 1 : 0

  name = "cre-security-attestation-note"

  attestation_authority {
    hint {
      human_readable_name = "CRE Security Scan Verification"
    }
  }
}

# KMS Keyring for Attestation Signing
resource "google_kms_key_ring" "attestor_keyring" {
  count = local.binauth_enabled ? 1 : 0

  name     = "cre-attestor-keyring"
  location = var.region
}

# KMS Key for Build Attestor
resource "google_kms_crypto_key" "build_attestor_key" {
  count = local.binauth_enabled ? 1 : 0

  name            = "cre-build-attestor-key"
  key_ring        = google_kms_key_ring.attestor_keyring[0].id
  purpose         = "ASYMMETRIC_SIGN"

  version_template {
    algorithm = "RSA_SIGN_PKCS1_4096_SHA512"
  }

  lifecycle {
    prevent_destroy = true
  }
}

# KMS Key for Security Attestor
resource "google_kms_crypto_key" "security_attestor_key" {
  count = local.binauth_enabled ? 1 : 0

  name            = "cre-security-attestor-key"
  key_ring        = google_kms_key_ring.attestor_keyring[0].id
  purpose         = "ASYMMETRIC_SIGN"

  version_template {
    algorithm = "RSA_SIGN_PKCS1_4096_SHA512"
  }

  lifecycle {
    prevent_destroy = true
  }
}

# Data source for KMS key version (build)
data "google_kms_crypto_key_version" "attestor_key_version" {
  count = local.binauth_enabled ? 1 : 0

  crypto_key = google_kms_crypto_key.build_attestor_key[0].id
}

# Data source for KMS key version (security)
data "google_kms_crypto_key_version" "security_attestor_key_version" {
  count = local.binauth_enabled ? 1 : 0

  crypto_key = google_kms_crypto_key.security_attestor_key[0].id
}

# IAM: Allow CI/CD service account to create attestations
resource "google_binary_authorization_attestor_iam_member" "cicd_attestor" {
  count = local.binauth_enabled ? 1 : 0

  attestor = google_binary_authorization_attestor.build_attestor[0].name
  role     = "roles/binaryauthorization.attestorsVerifier"
  member   = "serviceAccount:${var.cicd_service_account_email}"
}

# IAM: Allow security scanner service account to create attestations
resource "google_binary_authorization_attestor_iam_member" "security_attestor" {
  count = local.binauth_enabled ? 1 : 0

  attestor = google_binary_authorization_attestor.security_attestor[0].name
  role     = "roles/binaryauthorization.attestorsVerifier"
  member   = "serviceAccount:${var.security_scanner_service_account_email}"
}

# Outputs
output "binauth_policy_id" {
  description = "Binary Authorization policy ID"
  value       = local.binauth_enabled ? google_binary_authorization_policy.cre_policy[0].id : null
}

output "build_attestor_name" {
  description = "Build attestor name for CI/CD integration"
  value       = local.binauth_enabled ? google_binary_authorization_attestor.build_attestor[0].name : null
}

output "security_attestor_name" {
  description = "Security attestor name for vulnerability scanning"
  value       = local.binauth_enabled ? google_binary_authorization_attestor.security_attestor[0].name : null
}

output "attestor_key_ring" {
  description = "KMS keyring for attestation signing"
  value       = local.binauth_enabled ? google_kms_key_ring.attestor_keyring[0].id : null
}
