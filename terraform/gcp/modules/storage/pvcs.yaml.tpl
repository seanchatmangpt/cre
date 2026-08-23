# terraform/gcp/modules/storage/pvcs.yaml.tpl
# PVC manifests for CRE deployment
# Apply with: kubectl apply -f pvcs.yaml

---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: ${cluster_name}-mnesia-data
  namespace: cre
  labels:
    app: cre
    component: mnesia
    type: data
spec:
  accessModes:
    - ReadWriteOnce
  resources:
    requests:
      storage: 100Gi
  storageClassName: ${cluster_name}-ssd-regional

---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: ${cluster_name}-mnesia-logs
  namespace: cre
  labels:
    app: cre
    component: mnesia
    type: logs
spec:
  accessModes:
    - ReadWriteOnce
  resources:
    requests:
      storage: 50Gi
  storageClassName: ${cluster_name}-ssd

---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: ${cluster_name}-cre-data
  namespace: cre
  labels:
    app: cre
    component: data
spec:
  accessModes:
    - ReadWriteOnce
  resources:
    requests:
      storage: 200Gi
  storageClassName: ${cluster_name}-balanced
