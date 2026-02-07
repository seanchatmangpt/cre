terraform {
  backend "gcs" {
    bucket = "my-terraform-state-bucket"
    prefix = "gke-cluster"
  }
}
