# -----------------------------------------------------------------------------
# Outputs - Binary Authorization Module
# -----------------------------------------------------------------------------

output "policy_id" {
  description = "Binary Authorization policy ID"
  value       = google_binary_authorization_policy.cre_policy.name
}

output "policy_enforcement_mode" {
  description = "Current enforcement mode"
  value       = var.enforcement_mode
}

output "cosign_public_key_id" {
  description = "Cosign public key ID used for signature verification"
  value       = var.cosign_public_key_id
}

output "verification_example" {
  description = "Example command to verify image signature"
  value       = <<-EOT
# Verify CRE image signature
cosign verify us-central1-docker.pkg.dev/${var.project_id}/cre/cre:TAG \
  --key ${var.cosign_public_key_id}
EOT
}

output "test_deployment_command" {
  description = "Command to test Binary Authorization enforcement"
  value       = <<-EOT
# Attempt to deploy unsigned image (should fail if enforcement is enabled)
kubectl run test-unsigned --image=us-central1-docker.pkg.dev/${var.project_id}/cre/cre:test --dry-run=server

# Deploy signed image (should succeed)
kubectl run test-signed --image=us-central1-docker.pkg.dev/${var.project_id}/cre/cre:latest --dry-run=server
EOT
}
