# terraform/gcp/modules/storage/storage-classes.yaml.tpl
# StorageClass manifests for CRE deployment
# Apply with: kubectl apply -f storage-classes.yaml

---
apiVersion: storage.k8s.io/v1
kind: StorageClass
metadata:
  name: ${cluster_name}-ssd
  labels:
    app: cre
    managed-by: terraform
provisioner: kubernetes.io/gce-pd
parameters:
  type: pd-ssd
  fstype: ext4
  replication-type: none
volumeBindingMode: WaitForFirstConsumer
allowVolumeExpansion: true
reclaimPolicy: Delete

---
apiVersion: storage.k8s.io/v1
kind: StorageClass
metadata:
  name: ${cluster_name}-ssd-regional
  labels:
    app: cre
    managed-by: terraform
provisioner: kubernetes.io/gce-pd
parameters:
  type: pd-ssd
  fstype: ext4
  replication-type: regional-pd
volumeBindingMode: WaitForFirstConsumer
allowVolumeExpansion: true
reclaimPolicy: Delete

---
apiVersion: storage.k8s.io/v1
kind: StorageClass
metadata:
  name: ${cluster_name}-balanced
  labels:
    app: cre
    managed-by: terraform
provisioner: kubernetes.io/gce-pd
parameters:
  type: pd-balanced
  fstype: ext4
  replication-type: none
volumeBindingMode: WaitForFirstConsumer
allowVolumeExpansion: true
reclaimPolicy: Delete

---
apiVersion: storage.k8s.io/v1
kind: StorageClass
metadata:
  name: ${cluster_name}-standard
  labels:
    app: cre
    managed-by: terraform
provisioner: kubernetes.io/gce-pd
parameters:
  type: pd-standard
  fstype: ext4
  replication-type: none
volumeBindingMode: WaitForFirstConsumer
allowVolumeExpansion: true
reclaimPolicy: Delete
