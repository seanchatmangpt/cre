# Secret Manager Integration
# Centralized secret storage with automatic rotation and Kubernetes integration

locals {
  secret_manager_enabled = var.enable_secret_manager

  # Define secrets to be managed
  managed_secrets = {
    # Database credentials
    "cloudsql-connection-string" = {
      replication_policy = "automatic"
      rotation_period    = "2592000s" # 30 days
      description        = "Cloud SQL connection string"
    }
    "cloudsql-password" = {
      replication_policy = "automatic"
      rotation_period    = "2592000s"
      description        = "Cloud SQL database password"
    }

    # API keys
    "external-api-key" = {
      replication_policy = "automatic"
      rotation_period    = "7776000s" # 90 days
      description        = "External service API key"
    }

    # OAuth credentials
    "oauth-client-secret" = {
      replication_policy = "automatic"
      rotation_period    = "7776000s"
      description        = "OAuth 2.0 client secret"
    }

    # Encryption keys
    "app-encryption-key" = {
      replication_policy = "automatic"
      rotation_period    = "7776000s"
      description        = "Application-level encryption key"
    }

    # TLS certificates
    "tls-certificate" = {
      replication_policy = "automatic"
      rotation_period    = "5184000s" # 60 days
      description        = "TLS certificate for HTTPS"
    }
    "tls-private-key" = {
      replication_policy = "automatic"
      rotation_period    = "5184000s"
      description        = "TLS private key"
    }
  }
}

# Secret Manager Secrets
resource "google_secret_manager_secret" "secrets" {
  for_each = local.secret_manager_enabled ? local.managed_secrets : {}

  secret_id = each.key

  replication {
    auto {}
  }

  rotation {
    rotation_period = each.value.rotation_period
  }

  labels = {
    managed_by = "terraform"
    component  = "cre"
    rotation   = "enabled"
  }

  lifecycle {
    prevent_destroy = true
  }
}

# IAM: Allow Workload Identity service accounts to access secrets
resource "google_secret_manager_secret_iam_member" "workload_access" {
  for_each = local.secret_manager_enabled ? {
    for pair in flatten([
      for secret_key in keys(local.managed_secrets) : [
        for wi_key, wi_config in local.workload_identities : {
          secret_key = secret_key
          wi_key     = wi_key
          email      = google_service_account.workload_identity[wi_key].email
        }
        if contains(wi_config.gsa_roles, "roles/secretmanager.secretAccessor")
      ]
    ]) : "${pair.secret_key}-${pair.wi_key}" => pair
  } : {}

  secret_id = google_secret_manager_secret.secrets[each.value.secret_key].id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${each.value.email}"
}

# External Secrets Operator Configuration
resource "local_file" "external_secrets_operator_values" {
  count = local.secret_manager_enabled ? 1 : 0

  filename = "${path.module}/../config/security/secret-manager/external-secrets-operator-values.yaml"
  content  = <<-EOT
    # Helm values for External Secrets Operator
    installCRDs: true

    replicaCount: 2

    serviceAccount:
      create: true
      name: external-secrets
      annotations:
        iam.gke.io/gcp-service-account: ${google_service_account.workload_identity["external-secrets"].email}

    webhook:
      create: true
      port: 9443

    certController:
      create: true

    # Security context
    securityContext:
      fsGroup: 65534
      runAsNonRoot: true
      runAsUser: 65534

    podSecurityContext:
      fsGroup: 65534
      runAsNonRoot: true
      runAsUser: 65534

    resources:
      requests:
        cpu: 100m
        memory: 128Mi
      limits:
        cpu: 500m
        memory: 256Mi
  EOT
}

# SecretStore CRD for GCP Secret Manager
resource "local_file" "secret_store_gcp" {
  count = local.secret_manager_enabled ? 1 : 0

  filename = "${path.module}/../config/security/secret-manager/secretstore-gcp.yaml"
  content  = <<-EOT
    apiVersion: external-secrets.io/v1beta1
    kind: SecretStore
    metadata:
      name: gcpsm-secret-store
      namespace: cre-system
    spec:
      provider:
        gcpsm:
          projectID: "${var.project_id}"
          auth:
            workloadIdentity:
              clusterLocation: ${var.region}
              clusterName: ${var.cluster_name}
              serviceAccountRef:
                name: external-secrets
    ---
    apiVersion: external-secrets.io/v1beta1
    kind: ClusterSecretStore
    metadata:
      name: gcpsm-cluster-secret-store
    spec:
      provider:
        gcpsm:
          projectID: "${var.project_id}"
          auth:
            workloadIdentity:
              clusterLocation: ${var.region}
              clusterName: ${var.cluster_name}
              serviceAccountRef:
                name: external-secrets
                namespace: external-secrets
  EOT
}

# ExternalSecret examples for each managed secret
resource "local_file" "external_secret_examples" {
  count = local.secret_manager_enabled ? 1 : 0

  filename = "${path.module}/../config/security/secret-manager/external-secrets.yaml"
  content  = <<-EOT
    # Cloud SQL credentials
    apiVersion: external-secrets.io/v1beta1
    kind: ExternalSecret
    metadata:
      name: cloudsql-credentials
      namespace: cre-system
    spec:
      refreshInterval: 1h
      secretStoreRef:
        name: gcpsm-secret-store
        kind: SecretStore
      target:
        name: cloudsql-credentials
        creationPolicy: Owner
      data:
      - secretKey: connection-string
        remoteRef:
          key: cloudsql-connection-string
      - secretKey: password
        remoteRef:
          key: cloudsql-password

    ---
    # External API credentials
    apiVersion: external-secrets.io/v1beta1
    kind: ExternalSecret
    metadata:
      name: external-api-credentials
      namespace: cre-system
    spec:
      refreshInterval: 1h
      secretStoreRef:
        name: gcpsm-secret-store
        kind: SecretStore
      target:
        name: external-api-credentials
        creationPolicy: Owner
      data:
      - secretKey: api-key
        remoteRef:
          key: external-api-key

    ---
    # OAuth credentials
    apiVersion: external-secrets.io/v1beta1
    kind: ExternalSecret
    metadata:
      name: oauth-credentials
      namespace: cre-system
    spec:
      refreshInterval: 1h
      secretStoreRef:
        name: gcpsm-secret-store
        kind: SecretStore
      target:
        name: oauth-credentials
        creationPolicy: Owner
      data:
      - secretKey: client-secret
        remoteRef:
          key: oauth-client-secret

    ---
    # TLS certificates
    apiVersion: external-secrets.io/v1beta1
    kind: ExternalSecret
    metadata:
      name: tls-certificates
      namespace: cre-system
    spec:
      refreshInterval: 6h
      secretStoreRef:
        name: gcpsm-secret-store
        kind: SecretStore
      target:
        name: tls-certificates
        type: kubernetes.io/tls
        creationPolicy: Owner
      data:
      - secretKey: tls.crt
        remoteRef:
          key: tls-certificate
      - secretKey: tls.key
        remoteRef:
          key: tls-private-key
  EOT
}

# Secrets CSI Driver Configuration (alternative to External Secrets)
resource "local_file" "secrets_csi_driver_example" {
  count = local.secret_manager_enabled ? 1 : 0

  filename = "${path.module}/../config/security/secret-manager/secrets-csi-driver-deployment.yaml"
  content  = <<-EOT
    # Example: Using Secrets Store CSI Driver with Secret Manager
    apiVersion: v1
    kind: ServiceAccount
    metadata:
      name: app-with-csi-secrets
      namespace: cre-system
      annotations:
        iam.gke.io/gcp-service-account: ${google_service_account.workload_identity["cre-app"].email}
    ---
    apiVersion: secrets-store.csi.x-k8s.io/v1
    kind: SecretProviderClass
    metadata:
      name: cre-secret-provider
      namespace: cre-system
    spec:
      provider: gcp
      parameters:
        secrets: |
          - resourceName: "projects/${var.project_id}/secrets/cloudsql-password/versions/latest"
            path: "db-password"
          - resourceName: "projects/${var.project_id}/secrets/external-api-key/versions/latest"
            path: "api-key"
    ---
    apiVersion: apps/v1
    kind: Deployment
    metadata:
      name: app-with-csi-secrets
      namespace: cre-system
    spec:
      replicas: 3
      selector:
        matchLabels:
          app: app-with-csi-secrets
      template:
        metadata:
          labels:
            app: app-with-csi-secrets
        spec:
          serviceAccountName: app-with-csi-secrets
          containers:
          - name: app
            image: ${var.artifact_registry_region}-docker.pkg.dev/${var.project_id}/cre/app:latest
            volumeMounts:
            - name: secrets-store-inline
              mountPath: "/mnt/secrets"
              readOnly: true
            env:
            - name: DB_PASSWORD_FILE
              value: "/mnt/secrets/db-password"
            - name: API_KEY_FILE
              value: "/mnt/secrets/api-key"
          volumes:
          - name: secrets-store-inline
            csi:
              driver: secrets-store.csi.k8s.io
              readOnly: true
              volumeAttributes:
                secretProviderClass: "cre-secret-provider"
  EOT
}

# Setup script for External Secrets Operator
resource "local_file" "setup_script" {
  count = local.secret_manager_enabled ? 1 : 0

  filename = "${path.module}/../scripts/setup-external-secrets.sh"
  content  = <<-EOT
    #!/bin/bash
    # Setup External Secrets Operator

    set -euo pipefail

    echo "Installing External Secrets Operator..."

    # Add Helm repo
    helm repo add external-secrets https://charts.external-secrets.io
    helm repo update

    # Create namespace
    kubectl create namespace external-secrets --dry-run=client -o yaml | kubectl apply -f -

    # Apply Kubernetes ServiceAccount with Workload Identity
    kubectl apply -f ../config/security/workload-identity/external-secrets-service-account.yaml

    # Install External Secrets Operator
    helm upgrade --install external-secrets \\
      external-secrets/external-secrets \\
      --namespace external-secrets \\
      --values ../config/security/secret-manager/external-secrets-operator-values.yaml \\
      --wait

    echo "Waiting for External Secrets Operator to be ready..."
    kubectl wait --for=condition=ready pod \\
      -l app.kubernetes.io/name=external-secrets \\
      -n external-secrets \\
      --timeout=300s

    # Apply SecretStore
    kubectl apply -f ../config/security/secret-manager/secretstore-gcp.yaml

    # Apply ExternalSecrets
    kubectl apply -f ../config/security/secret-manager/external-secrets.yaml

    echo "External Secrets Operator setup complete!"
    echo ""
    echo "Verify installation:"
    echo "  kubectl get secretstore -n cre-system"
    echo "  kubectl get externalsecret -n cre-system"
    echo "  kubectl get secret -n cre-system"
  EOT

  file_permission = "0755"
}

# Outputs
output "secret_manager_secrets" {
  description = "Secret Manager secret IDs"
  value       = local.secret_manager_enabled ? keys(google_secret_manager_secret.secrets) : []
}

output "secret_manager_project" {
  description = "Project ID for Secret Manager"
  value       = var.project_id
}
