# Workload Identity for GKE
# Enables GKE pods to access GCP services without service account keys

locals {
  workload_identity_enabled = var.enable_workload_identity

  # Define workload identity mappings
  workload_identities = {
    # CRE Application workload
    cre-app = {
      namespace           = "cre-system"
      ksa_name           = "cre-app"
      gsa_name           = "cre-app-workload"
      gsa_roles          = [
        "roles/secretmanager.secretAccessor",
        "roles/storage.objectViewer",
        "roles/cloudsql.client",
      ]
    }

    # Monitoring workload
    monitoring = {
      namespace           = "monitoring"
      ksa_name           = "prometheus"
      gsa_name           = "cre-monitoring-workload"
      gsa_roles          = [
        "roles/monitoring.metricWriter",
        "roles/logging.logWriter",
      ]
    }

    # Backup workload
    backup = {
      namespace           = "cre-system"
      ksa_name           = "backup-agent"
      gsa_name           = "cre-backup-workload"
      gsa_roles          = [
        "roles/storage.objectAdmin",
        "roles/cloudsql.admin",
      ]
    }

    # External Secrets Operator
    external-secrets = {
      namespace           = "external-secrets"
      ksa_name           = "external-secrets"
      gsa_name           = "cre-external-secrets-workload"
      gsa_roles          = [
        "roles/secretmanager.secretAccessor",
      ]
    }
  }
}

# Google Service Accounts for Workload Identity
resource "google_service_account" "workload_identity" {
  for_each = local.workload_identity_enabled ? local.workload_identities : {}

  account_id   = each.value.gsa_name
  display_name = "Workload Identity for ${each.key}"
  description  = "Service account for ${each.value.namespace}/${each.value.ksa_name} workload identity"
}

# IAM Policy Binding: GKE Service Account to Google Service Account
resource "google_service_account_iam_member" "workload_identity_binding" {
  for_each = local.workload_identity_enabled ? local.workload_identities : {}

  service_account_id = google_service_account.workload_identity[each.key].name
  role               = "roles/iam.workloadIdentityUser"
  member             = "serviceAccount:${var.project_id}.svc.id.goog[${each.value.namespace}/${each.value.ksa_name}]"
}

# IAM Policy: Assign roles to workload identity service accounts
resource "google_project_iam_member" "workload_identity_roles" {
  for_each = local.workload_identity_enabled ? merge([
    for key, config in local.workload_identities : {
      for role in config.gsa_roles :
      "${key}-${role}" => {
        key     = key
        role    = role
        member  = "serviceAccount:${google_service_account.workload_identity[key].email}"
      }
    }
  ]...) : {}

  project = var.project_id
  role    = each.value.role
  member  = each.value.member
}

# Kubernetes Service Account manifests (to be applied via kubectl)
resource "local_file" "kubernetes_service_accounts" {
  for_each = local.workload_identity_enabled ? local.workload_identities : {}

  filename = "${path.module}/../config/security/workload-identity/${each.key}-service-account.yaml"
  content  = <<-EOT
    apiVersion: v1
    kind: ServiceAccount
    metadata:
      name: ${each.value.ksa_name}
      namespace: ${each.value.namespace}
      annotations:
        iam.gke.io/gcp-service-account: ${google_service_account.workload_identity[each.key].email}
    ---
    apiVersion: v1
    kind: Namespace
    metadata:
      name: ${each.value.namespace}
  EOT
}

# Example deployment with Workload Identity
resource "local_file" "workload_identity_example" {
  count = local.workload_identity_enabled ? 1 : 0

  filename = "${path.module}/../config/security/workload-identity/example-deployment.yaml"
  content  = <<-EOT
    # Example: Deployment using Workload Identity
    apiVersion: apps/v1
    kind: Deployment
    metadata:
      name: cre-app
      namespace: cre-system
    spec:
      replicas: 3
      selector:
        matchLabels:
          app: cre-app
      template:
        metadata:
          labels:
            app: cre-app
        spec:
          serviceAccountName: cre-app  # ← Uses Workload Identity
          securityContext:
            runAsNonRoot: true
            runAsUser: 1000
            fsGroup: 1000
          containers:
          - name: cre-app
            image: ${var.artifact_registry_region}-docker.pkg.dev/${var.project_id}/cre/app:latest
            env:
            # No GOOGLE_APPLICATION_CREDENTIALS needed - Workload Identity handles auth
            - name: GCP_PROJECT_ID
              value: "${var.project_id}"
            securityContext:
              allowPrivilegeEscalation: false
              readOnlyRootFilesystem: true
              capabilities:
                drop:
                - ALL
            volumeMounts:
            - name: tmp
              mountPath: /tmp
          volumes:
          - name: tmp
            emptyDir: {}
  EOT
}

# Cloud SQL Proxy example with Workload Identity
resource "local_file" "cloudsql_proxy_example" {
  count = local.workload_identity_enabled ? 1 : 0

  filename = "${path.module}/../config/security/workload-identity/cloudsql-proxy-deployment.yaml"
  content  = <<-EOT
    # Example: Cloud SQL Proxy with Workload Identity
    apiVersion: apps/v1
    kind: Deployment
    metadata:
      name: cre-app-with-cloudsql
      namespace: cre-system
    spec:
      replicas: 3
      selector:
        matchLabels:
          app: cre-app-cloudsql
      template:
        metadata:
          labels:
            app: cre-app-cloudsql
        spec:
          serviceAccountName: cre-app  # ← Workload Identity enabled
          containers:
          - name: cre-app
            image: ${var.artifact_registry_region}-docker.pkg.dev/${var.project_id}/cre/app:latest
            env:
            - name: DB_HOST
              value: "127.0.0.1"
            - name: DB_PORT
              value: "5432"
            - name: DB_NAME
              valueFrom:
                secretKeyRef:
                  name: cloudsql-db-credentials
                  key: database

          # Cloud SQL Proxy sidecar (no credentials required)
          - name: cloud-sql-proxy
            image: gcr.io/cloud-sql-connectors/cloud-sql-proxy:latest
            args:
            - "--structured-logs"
            - "--port=5432"
            - "${var.project_id}:${var.region}:${var.cloudsql_instance_name}"
            securityContext:
              runAsNonRoot: true
              allowPrivilegeEscalation: false
            resources:
              requests:
                memory: "256Mi"
                cpu: "100m"
              limits:
                memory: "512Mi"
                cpu: "200m"
  EOT
}

# README for Workload Identity setup
resource "local_file" "workload_identity_readme" {
  count = local.workload_identity_enabled ? 1 : 0

  filename = "${path.module}/../config/security/workload-identity/README.md"
  content  = <<-EOT
    # Workload Identity Configuration

    ## Overview

    Workload Identity enables GKE pods to access GCP services without service account keys.

    ## Setup Instructions

    ### 1. Enable Workload Identity on GKE Cluster

    The cluster is already configured with Workload Identity. Verify:

    ```bash
    gcloud container clusters describe ${var.cluster_name} \
      --region=${var.region} \
      --format="value(workloadIdentityConfig.workloadPool)"
    ```

    Expected output: `${var.project_id}.svc.id.goog`

    ### 2. Apply Kubernetes Service Accounts

    ```bash
    kubectl apply -f config/security/workload-identity/
    ```

    ### 3. Verify Bindings

    Check that the Kubernetes ServiceAccount is properly annotated:

    ```bash
    kubectl get sa cre-app -n cre-system -o yaml
    ```

    Look for the annotation:
    ```yaml
    iam.gke.io/gcp-service-account: ${google_service_account.workload_identity["cre-app"].email}
    ```

    ### 4. Test Workload Identity

    Deploy a test pod and verify GCP access:

    ```bash
    kubectl run -it --rm --restart=Never \
      --serviceaccount=cre-app \
      --namespace=cre-system \
      --image=google/cloud-sdk:alpine \
      test-workload-identity \
      -- gcloud auth list
    ```

    Expected output should show the workload identity service account.

    ## Configured Workload Identities

    ${join("\n", [for key, config in local.workload_identities : "- **${key}**: ${config.namespace}/${config.ksa_name} → ${config.gsa_name}"])}

    ## Security Best Practices

    1. **Least Privilege**: Each workload has only the permissions it needs
    2. **No Keys**: No service account keys are created or stored
    3. **Automatic Rotation**: Credentials are automatically rotated by GKE
    4. **Audit Trail**: All GCP API calls are logged with the service account identity

    ## Troubleshooting

    ### Pod cannot authenticate to GCP

    1. Verify the ServiceAccount annotation:
       ```bash
       kubectl describe sa <ksa-name> -n <namespace>
       ```

    2. Check IAM binding:
       ```bash
       gcloud iam service-accounts get-iam-policy \\
         <gsa-email> \\
         --format=json
       ```

    3. Verify Workload Identity is enabled on the node pool:
       ```bash
       kubectl get nodes -o json | jq '.items[].metadata.labels["iam.gke.io/gke-metadata-server-enabled"]'
       ```

       Should return "true" for all nodes.

    ### Permission Denied errors

    1. Check that the Google Service Account has the required roles:
       ```bash
       gcloud projects get-iam-policy ${var.project_id} \\
         --flatten="bindings[].members" \\
         --filter="bindings.members:serviceAccount:<gsa-email>"
       ```

    2. Verify the pod is using the correct ServiceAccount:
       ```bash
       kubectl get pod <pod-name> -n <namespace> -o yaml | grep serviceAccountName
       ```

    ## References

    - [GKE Workload Identity Documentation](https://cloud.google.com/kubernetes-engine/docs/how-to/workload-identity)
    - [Best Practices](https://cloud.google.com/kubernetes-engine/docs/how-to/workload-identity#best_practices)
  EOT
}

# Outputs
output "workload_identity_service_accounts" {
  description = "Workload Identity Google Service Accounts"
  value = local.workload_identity_enabled ? {
    for key, sa in google_service_account.workload_identity :
    key => {
      email      = sa.email
      name       = sa.name
      namespace  = local.workload_identities[key].namespace
      ksa_name   = local.workload_identities[key].ksa_name
    }
  } : {}
}

output "workload_identity_pool" {
  description = "Workload Identity Pool"
  value       = "${var.project_id}.svc.id.goog"
}
